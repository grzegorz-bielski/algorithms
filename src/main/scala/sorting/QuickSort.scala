package sorting

import Ordering.Implicits.infixOrderingOps
import scala.reflect.ClassTag

object InmuttableQuickSort:
  def sort[T: Ordering: ClassTag](arr: Array[T]): Array[T] =
    if arr.length <= 1 then arr
    else
      val pivot = arr.middle
      sort(arr.filter(pivot > _)) ++
        arr.filter(pivot == _) ++
        sort(arr.filter(pivot < _))

object InPlaceQuickSort:
  // https://www.geeksforgeeks.org/quick-sort/
  def sort[T: Ordering](arr: Array[T], low: Int, high: Int): Array[T] =
    if low < high then
      val pivot = partition(arr, low, high)
      sort(arr, low, pivot - 1) // before pivot
      sort(arr, pivot + 1, high) // after pivot
      arr
    else arr

  // not sure if this is correct, does it even solve it recursively?
  private def partition[T: Ordering](arr: Array[T], low: Int, high: Int): Int =
    val pivotElement = arr(high)
    val i = (low to high).foldLeft(low - 1) { (i, j) =>
      if arr(j) <= pivotElement then
        val next = i + 1 // 0 during first iteration
        arr.swap(next, j)
        next
      else i
    }

    val next = i + 1
    arr.swap(next, high) // put pivot in a correct spot
    next

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

object InPlaceQuickSort2:
    // https://www.youtube.com/watch?v=Hoixgm4-P4M
    def sort[T: Ordering](arr: Array[T], low: Int, high: Int): Array[T] = ???
