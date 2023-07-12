package algos.sorting

import algos.*

import scala.reflect.ClassTag
import Ordering.Implicits.given

// for every element in the array, compare it to the next element, and swap if the next element is bigger
// after one full iteration the largest element will be at the end of the array
// repeat until (n -1)

// (n + 1) * n / 2 -> O(n^2) in worst case

object BubbleSort:
  def sort[T: Ordering: ClassTag](_items: Array[T]): Array[T] =
    val items = _items.clone

    for
      i <- 0 until items.length
      j <- 0 until items.length - 1 - i
      if items(j) > items(j + 1)
    yield items.swap_(j, j + 1)

    items
