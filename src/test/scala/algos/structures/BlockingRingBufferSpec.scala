package algos.structures

import org.scalatest.*, funsuite.*, matchers.*
import java.util.concurrent.Executors
import java.util.concurrent.atomic.AtomicInteger
import scala.language.unsafeNulls
import scala.util.Using
import java.util.concurrent.ExecutorService
import java.util.concurrent.CyclicBarrier

class BlockingRingBufferSpec extends AnyFunSuite, should.Matchers:
  test("should pass the basic test workflow"):
    val buffer = BlockingRingBuffer[Int](5)

    buffer.push(123)
    buffer.push(456)
    buffer.push(789)
    buffer.push(666)
    //   buffer.push(6667)
    //   buffer.push(6667) // blocking

    buffer.pop() shouldBe 123
    buffer.pop() shouldBe 456
    buffer.pop() shouldBe 789

    buffer.push(333)
    buffer.push(555)

    buffer.pop() shouldBe 666
    buffer.pop() shouldBe 333
    buffer.pop() shouldBe 555
  //   buffer.pop // blocking

  test("blocks when necessary while being responsive to interruption"):
    val buffer = BlockingRingBuffer[Int](10)

    val LOCKUP_DETECT_TIMEOUT = 1000L

    val taker = new Thread:
      override def run(): Unit =
        try
          buffer.pop() // blocks on empty buffer
          fail("should not reach here")
        catch case _: InterruptedException => ()

    taker.start()
    Thread.sleep(LOCKUP_DETECT_TIMEOUT)
    taker.interrupt()
    taker.join(LOCKUP_DETECT_TIMEOUT)

  test("should pass the concurrent workflow"):
    // Compute checksums of the elements that are enqueued and dequeued using order insensitive, commutative function (xorshift)

    given Using.Releasable[ExecutorService] = _.shutdown()

    val putSum = AtomicInteger(0)
    val takeSum = AtomicInteger(0)
    val capacity = 10
    val nPairs = 10
    val nTrials = 100000
    // start and finish of nPairs + final barrier
    val barrier = CyclicBarrier(nPairs * 2 + 1)

    val buffer = BlockingRingBuffer[Int](capacity)

    // good enough PRNG
    def xorShift(y: Int): Int =
      var x = y
      x ^= x << 6
      x ^= x >>> 21
      x ^= x << 7
      x

    final class Producer extends Runnable:
      override def run(): Unit =
        var seed = hashCode() ^ System.nanoTime().toInt
        var sum = 0
        barrier.await()
        (nTrials until 0 by -1).foreach: _ =>
          buffer.push(seed)
          sum += seed
          seed = xorShift(seed)
        putSum.getAndAdd(sum)
        barrier.await()
        ()

    final class Consumer extends Runnable:
      override def run(): Unit =
        barrier.await()
        var sum = 0
        (nTrials until 0 by -1).foreach: _ =>
          sum += buffer.pop()
        takeSum.getAndAdd(sum)
        barrier.await()
        ()

    Using(Executors.newCachedThreadPool()): pool =>
      (0 until nPairs).foreach: _ =>
        pool.submit(Producer())
        pool.submit(Consumer())

      barrier.await() // wait for all threads to be ready
      barrier.await() // wait for all threads to finish
    .fold(
      err => fail(err),
      _ => putSum.get shouldBe takeSum.get
    )
