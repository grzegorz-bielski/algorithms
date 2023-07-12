package algos.search

import scala.annotation.tailrec

object BinarySearch:

  // https://leetcode.com/problems/binary-search/
  def apply(items: Array[Int], item: Int): Int = 
    search[Int](items, item).getOrElse(-1)

  def search[T: Ordering](items: Array[T], item: T): Option[Int] =

    @tailrec
    def go(low: Int, high: Int): Option[Int] =
      if low > high then None
      else
        val mid = Math.floor((low + high) / 2).toInt
        val guess = items(mid)
        val comp = Ordering[T].compare(guess, item)

        comp.sign match
          case -1 => go(mid + 1, high)
          case 0  => Some(mid)
          case 1  => go(low, mid - 1)

    go(0, items.length - 1)


  