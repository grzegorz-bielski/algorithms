package algos.sorting

import org.scalatest.*, funsuite.*, matchers.*

class BubbleSortSpec extends AnyFunSuite, should.Matchers:
  val unsortedItems = Array(2, 6, 5, 3, 8, 7, 1, 0)
  val sortedItems = Array(0, 1, 2, 3, 5, 6, 7, 8)

  test("sorts items correctly"):
    BubbleSort.sort(unsortedItems) shouldBe sortedItems
