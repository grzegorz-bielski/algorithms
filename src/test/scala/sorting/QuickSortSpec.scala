package sorting

import org.scalatest.*
import flatspec.*
import matchers.*

class QuickSortSpec extends AnyFlatSpec, should.Matchers:
  val unsortedItems = Array(2, 6, 5, 3, 8, 7, 1, 0)
  val sortedItems = Array(0, 1, 2, 3, 5, 6, 7, 8)

  "HighPivotSort" should "sort items correctly" in:
    InPlaceQuickSort.highPivotSort(unsortedItems) shouldBe sortedItems

  "LeftWallQucikSort" should "sort items correctly" in:
    InPlaceQuickSort.leftWallSort(unsortedItems) shouldBe sortedItems

  "ImmutableQuickSort" should "sort items correctly" in:
    ImmutableQuickSort.sort(unsortedItems) shouldBe sortedItems
