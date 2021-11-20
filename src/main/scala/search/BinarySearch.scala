package search

import scala.annotation.tailrec

object BinarySearch:
  def apply[T: Ordering](items: Seq[T], item: T): Option[Int] =

    @tailrec
    def go(low: Int, high: Int): Option[Int] =
      if low > high then None
      else
        val mid = low + high
        val guess = items(mid)
        val comp = implicitly[Ordering[T]].compare(guess, item)

        comp.sign match
          case -1 => go(mid + 1, high)
          case 0  => Some(mid)
          case 1  => go(low, mid - 1)

    go(0, items.length - 1)

