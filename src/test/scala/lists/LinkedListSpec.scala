package lists

import org.scalatest.*
import flatspec.*
import matchers.*

class LinkedListSpec extends AnyFlatSpec with should.Matchers:
  "from" should "create list from interable" in {
    LinkedList.from(0 to 4) shouldBe LinkedList(0, 1, 2, 3, 4)
  }

  "get" should "return element at index" in {
    LLNil.get(4) shouldBe None
    LinkedList(1, 2, 3).get(1) shouldBe Some(2)
  }

  "custom unapply" should "correctly work with pattern matching" in {
    val list = 1 :: 2 :: 3 :: LLNil
    val result = list match
      case LinkedList(a, b, c) => (a, b, c)
      case _                   => (3, 2, 1)

    result shouldBe (1, 2, 3)
  }

  "length" should "correctly calculate list size" in {
    LLNil.length shouldBe 0
    LinkedList(1, 2, 3).length shouldBe 3
  }

  "reverse" should "reverse a linked list" in {
    LLNil.reverse shouldBe LLNil
    LinkedList(1, 2, 3).reverse shouldBe LinkedList(3, 2, 1)
  }

  "concat" should "concatenate two list" in {
    (LinkedList(1, 2, 3) ++ LinkedList(4, 5)) shouldBe LinkedList(1, 2, 3, 4, 5)
  }
