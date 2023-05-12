package search

import org.scalatest.*
import flatspec.*
import matchers.*

class BinarySearchSpec extends AnyFlatSpec, should.Matchers:
  "apply" should "find value at valid index" in:
    BinarySearch.search(
      Array(2, 6, 4, 10, 34),
      4
    ) should be(Some(2))

  "apply" should "return None when value is not in the list" in:
    BinarySearch.search(
      Array(2, 6, 4, 10, 34),
      46
    ) should be(None)
