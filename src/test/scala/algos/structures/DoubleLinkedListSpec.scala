package algos.structures

import org.scalatest.*, funsuite.*, matchers.*

class DoubleLinkedListSpec extends AnyFunSuite, should.Matchers:
  test("should pass the test workflow"):
    val l = DoubleLinkedList[Int]()

    l.prepend(1)
    l.prepend(2)
    l.append(3)
    l.toList shouldBe List(2, 1, 3)
    l.get(0) shouldBe Some(2)
    l.get(1) shouldBe Some(1)
    l.get(2) shouldBe Some(3)
    l.removeAt(1) shouldBe Some(1)
    l.removeAt(3) shouldBe None
    l.length shouldBe 2
    l.append(11)
    l.removeAt(1) shouldBe Some(3)
    l.toList shouldBe List(2, 11)
    l.remove(2) shouldBe Some(2)
    l.toList shouldBe List(11)
    l.find(2) shouldBe None
    l.find(11) shouldBe Some(11)
    l.removeAt(0) shouldBe Some(11)
    l.get(0) shouldBe None
    l.prepend(42)
    l.get(0) shouldBe Some(42)
    l.get(11) shouldBe None
