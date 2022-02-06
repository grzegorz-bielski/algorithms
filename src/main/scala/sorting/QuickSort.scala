package sorting

import Ordering.Implicits.infixOrderingOps
import scala.reflect.ClassTag

object ImmutableQuickSort:
  // nice, but naive
  def sort[T: Ordering: ClassTag](arr: Array[T]): Array[T] =
    if arr.length <= 1 then arr
    else
      val pivot = arr.middle
      sort(arr.filter(pivot > _)) ++
        arr.filter(pivot == _) ++
        sort(arr.filter(pivot < _))

object InPlaceQuickSort:
  // based on https://algorithmist.com/wiki/Quicksort
  def leftWallSort[T: Ordering](arr: Array[T]) = sort[T] { case (arr, low, high) =>
    val pivotIndex = low
    val pivot = arr(pivotIndex)
    val leftWall = (low + 1 to high).foldLeft(low) { (leftWall, i) =>
      if arr(i) < pivot then
        val nextLeftWall = leftWall + 1
        arr.swap(i, nextLeftWall)
        nextLeftWall
      else leftWall
    }

    arr.swap(pivotIndex, leftWall)
    leftWall
  }(arr)

  // based on https://www.geeksforgeeks.org/quick-sort/
  // not sure if this is correct, does it even solve it recursively rather than in one iteration?
  def highPivotSort[T: Ordering](arr: Array[T]) = sort[T] { case (arr, low, high) =>
    val pivot = arr(high)
    val i = (low to high).foldLeft(low - 1) { (i, j) =>
      if arr(j) < pivot then
        val next = i + 1 // 0 during first iteration
        arr.swap(next, j)
        next
      else i
    }

    val next = i + 1
    arr.swap(next, high) // put pivot in a correct spot
    next
  }(arr)

  type Partition[T] = Ordering[T] ?=> (Array[T], Int, Int) => Int

  private def sort[T: Ordering](partition: Partition[T])(arr: Array[T]): Array[T] =
    val copied = arr.clone // immutable public api

    // this is not tailrec
    // should use some trampoling: https://stackoverflow.com/questions/9247504/how-to-implement-tail-recursive-quick-sort-in-scala
    def go(arr: Array[T], low: Int, high: Int): Array[T] =
      if low < high then
        val pivot = partition(arr, low, high)
        go(arr, low, pivot - 1) // before pivot
        go(arr, pivot + 1, high) // after pivot
        arr
      else arr

    go(copied, 0, copied.size - 1)

extension [T](a: Array[T])
  def swap(i: Int, j: Int): Array[T] =
    val iv = a(i)
    val jv = a(j)
    a(i) = jv
    a(j) = iv
    a

  def middle =
    val mid = Math.floor(a.length / 2).toInt
    a(mid)
