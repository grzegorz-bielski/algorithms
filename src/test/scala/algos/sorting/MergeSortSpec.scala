package algos.sorting

import org.scalatest.*, funsuite.*, matchers.*

class MergeSortSpec extends AnyFunSuite, should.Matchers:
  val unsortedItems = List(2, 6, 5, 3, 8, 7, 1, 0)
  val sortedItems = List(0, 1, 2, 3, 5, 6, 7, 8)

  test("sorts items correctly"):
    MergeSort.sort(unsortedItems) shouldBe sortedItems
