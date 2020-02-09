package search

import org.scalatest._

class BinarySearchSpec extends FlatSpec with Matchers {
  "search" should "find value at valid index" in {

    BinarySearch.search(
      List(2, 6, 4, 10, 34),
      4
    ) should be(Some(2))
  }

  "search" should "return None when value is not in the list" in {
    BinarySearch.search(
      List(2, 6, 4, 10, 34),
      46
    ) should be(None)
  }
}
