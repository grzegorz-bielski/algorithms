package search

import org.scalatest._
import flatspec._
import matchers._

class BinarySearchSpec extends AnyFlatSpec with should.Matchers {
  "apply" should "find value at valid index" in {
    BinarySearch(
      List(2, 6, 4, 10, 34),
      4
    ) should be(Some(2))
  }

  "apply" should "return None when value is not in the list" in {
    BinarySearch(
      List(2, 6, 4, 10, 34),
      46
    ) should be(None)
  }
}
