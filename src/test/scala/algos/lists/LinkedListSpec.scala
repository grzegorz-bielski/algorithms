package algos.lists

import org.scalatest.*
import flatspec.*
import matchers.*

class LinkedListSpec extends AnyFlatSpec, should.Matchers:
  "from" should "create list from interable" in:
    LinkedList.from(0 to 4) shouldBe LinkedList(0, 1, 2, 3, 4)

  "get" should "return element at index" in:
    LLNil.get(4) shouldBe None
    LinkedList(1, 2, 3).get(1) shouldBe Some(2)

  "custom unapply" should "correctly work with pattern matching" in:
    val list = 1 :: 2 :: 3 :: LLNil
    val result = list match
      case LinkedList(a, b, c) => (a, b, c)
      case _                   => (3, 2, 1)

    result shouldBe (1, 2, 3)

  "length" should "correctly calculate list size" in:
    LLNil.length shouldBe 0
    LinkedList(1, 2, 3).length shouldBe 3

  "reverse" should "reverse a linked list" in:
    LLNil.reverse shouldBe LLNil
    LinkedList(1, 2, 3).reverse shouldBe LinkedList(3, 2, 1)

  "concat" should "concatenate two list" in:
    (LinkedList(1, 2, 3) ++ LinkedList(4, 5)) shouldBe LinkedList(1, 2, 3, 4, 5)
  
  "removeAt" should "remove element at specific index" in:
    LinkedList(1, 2, 3, 4, 5).removeAt(2) shouldBe LinkedList(1, 2, 4, 5)
    LinkedList(1).removeAt(2) shouldBe LinkedList(1)

  "flatMap" should "correctly apply provided function" in:
    LinkedList(1, 2, 3).flatMap(n => LinkedList(n, 7)) shouldBe LinkedList(1, 7, 2, 7, 3, 7)

  "map" should "correctly apply provided function" in:
    LinkedList(1, 2, 3).flatMap(n => LinkedList(n + 1)) shouldBe LinkedList(2, 3, 4)

  "filter" should "correctly apply provided function" in:
    LinkedList(1, 1, 2, 3).filter(_ == 1) shouldBe LinkedList(1, 1)
