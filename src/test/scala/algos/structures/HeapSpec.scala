package algos.structures

import org.scalatest.*, funsuite.*, matchers.*

class HeapSpec extends AnyFunSuite, should.Matchers:
  test("should pass test workflow"):
    val heap = MinHeap.empty[Int]

    heap.insert(5)
    heap.insert(3)
    heap.insert(69)
    heap.insert(420)
    heap.insert(4)
    heap.insert(1)
    heap.insert(8)
    heap.insert(7)

    heap.length shouldBe 8
    heap.pop() shouldBe Some(1)
    heap.pop() shouldBe Some(3)
    heap.pop() shouldBe Some(4)
    heap.pop() shouldBe Some(5)
    heap.length shouldBe 4
    heap.pop() shouldBe Some(7)
    heap.pop() shouldBe Some(8)
    heap.pop() shouldBe Some(69)
    heap.pop() shouldBe Some(420)
    heap.length shouldBe 0
    heap.pop() shouldBe None
