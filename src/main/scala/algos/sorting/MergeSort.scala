package algos.sorting

import scala.util.control.TailCalls.*
import Ordering.Implicits.given

// 1. divide collection in 2 equally sized parts until you reach pairs
// 2. calls itself recursively for each half
// 3. merge results

// always O(n log n)
// stable
// equal sublists
// most of the work happens in merge step

// good for linked lists, it doesn't need random access

object MergeSort:
  def sort[T: Ordering](xs: List[T]): List[T] =
    def go[T: Ordering](xs: List[T]): TailRec[List[T]] = xs match
      case Nil      => done(xs)
      case _ +: Nil => done(xs)
      case _ =>
        val (left, right) = xs.splitAt(xs.length / 2)
        for
          sortedLeft <- go(left)
          sortedRight <- go(right)
          merged <- merge(sortedLeft, sortedRight)
        yield merged

    go(xs).result

  private def merge[T: Ordering](left: List[T], right: List[T]): TailRec[List[T]] = (left, right) match
    case (Nil, _) => done(right)
    case (_, Nil) => done(left)
    case (l :: leftTail, r :: rightTail) =>
      if l < r
      then tailcall(merge(leftTail, right)).map(l +: _) // move to left
      else tailcall(merge(left, rightTail)).map(r +: _) // move to right
