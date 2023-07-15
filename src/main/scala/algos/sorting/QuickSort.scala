package algos.sorting

import algos.*
import scala.util.control.TailCalls.*

import Ordering.Implicits.given
import scala.reflect.ClassTag

// 1. pick a pivot
// 2. partition array in 2 parts, with elements smaller and bigger than pivot
// 3. recursively sort each part
// 4. join parts

// best case O(n log n), worst case O(n^2)
// not stable
// not necessarily equal sublists
// most of the work happens in partition step

// (in place version) - good for small array and limited memory

object ImmutableQuickSort:
  // nice, but naive
  // 3 filters in the same iteration, not stack safe
  def strictSort[T: Ordering: ClassTag](arr: Array[T]): Array[T] =
    if arr.length <= 1 then arr
    else
      val pivot = arr.middle
      strictSort(arr.filter(pivot > _)) ++
        arr.filter(pivot == _) ++
        strictSort(arr.filter(pivot < _))

  // haskell like, stack safe, Lazy Lists elements should be memoized to avoid recomputing
  // still more overhead than in-place 
  def lazySort[T: Ordering: ClassTag](arr: LazyList[T]): LazyList[T] =
    if arr.isEmpty then arr
    else
      val pivot = arr.head
      val (smaller, bigger) = arr.tail.partition(_ < pivot)
      lazySort(smaller) #::: pivot #:: lazySort(bigger)

object InPlaceQuickSort:
  // based on https://algorithmist.com/wiki/Quicksort
  def leftWallSort[T: Ordering](arr: Array[T]) = sort[T](arr): (arr, low, high) =>
    val pivotIndex = low
    val pivot = arr(pivotIndex)
    val leftWall = (low + 1 to high).foldLeft(low): (leftWall, i) =>
      if arr(i) < pivot then
        val nextLeftWall = leftWall + 1
        arr.swap(i, nextLeftWall)
        nextLeftWall
      else leftWall

    arr.swap(pivotIndex, leftWall)
    leftWall

  // based on https://www.geeksforgeeks.org/quick-sort/
  def highPivotSort[T: Ordering](arr: Array[T]) = sort[T](arr): (arr, low, high) =>
    val pivot = arr(high)
    val i = (low to high).foldLeft(low - 1): (i, j) =>
      if arr(j) < pivot then
        val next = i + 1 // 0 during first iteration
        arr.swap(next, j)
        next
      else i

    val next = i + 1
    arr.swap(next, high) // put pivot in a correct spot
    next

  type Partition[T] = (Array[T], Int, Int) => Int

  // O(n log n)
  // n * half the space from prev iteration until 1
  private def sort[T: Ordering](arr: Array[T])(partition: Partition[T]): Array[T] =
    val copied = arr.clone // immutable public api

    def go(arr: Array[T], low: Int, high: Int): TailRec[Array[T]] =
      if low < high then
        val pivot = partition(arr, low, high)
        for
          _ <- tailcall(go(arr, low, pivot - 1)) // before pivot
          _ <- tailcall(go(arr, pivot + 1, high)) // after pivot
        yield arr
      else done(arr)

    go(copied, 0, copied.size - 1).result
