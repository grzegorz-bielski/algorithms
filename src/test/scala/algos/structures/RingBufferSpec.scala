package algos.structures

import org.scalatest.*, funsuite.*, matchers.*

class RingBufferSpec extends AnyFunSuite, should.Matchers:
    test("should pass the test workflow"):
      val buffer = RingBuffer[Int](5)

      buffer.push(123) shouldBe true
      buffer.push(456) shouldBe true
      buffer.push(789) shouldBe true
      buffer.push(666) shouldBe true
      
      buffer.pop shouldBe Some(123)
      buffer.pop shouldBe Some(456)
      buffer.pop shouldBe Some(789)

      buffer.push(333) shouldBe true
      buffer.push(555) shouldBe true

      buffer.pop shouldBe Some(666)
      buffer.pop shouldBe Some(333)
      buffer.pop shouldBe Some(555)
      buffer.pop shouldBe None

