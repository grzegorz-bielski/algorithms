package algos.structures

import scala.collection.mutable.ArrayBuffer
import algos.*
import Ordering.Implicits.given
import scala.reflect.ClassTag

/** Self-balancing Priority Queue implementation of Binary Tree backed by ArrayList / ArrayBuffer
  *
  * Guarantees the following Heap condition:
  *   - MaxHeap: every child and grandchild is smaller than the current node
  *   - _MinHeap_: every child and grandchild is larger than the current node -> .pop() returns smallest element
  *
  * Maintains a partial ordering of elements. Not thread-safe.
  */
final class MinHeap[T: Ordering]:
  private var treeHeight = 0
  private val data = ArrayBuffer.empty[T]

  def length: Int = treeHeight

  // O(log n) -> half of the tree is always at the bottom layer
  // 1. add to end of array
  // 2. heapify up
  def insert(value: T): Unit =
    data += value
    heapifyUp(treeHeight)
    treeHeight += 1

  // O(log n)
  // 1. put last element at the head
  // 2. heapify down
  // 3. return head
  def pop(): Option[T] =
    if treeHeight == 0 then None
    else
      val out = data.headOption
      treeHeight -= 1

      if treeHeight == 0 then
        data.clear()
        out
      else
        data(0) = data(treeHeight)
        heapifyDown(0)
        out

  // 1. if parent is smaller than child, swap
  // 2. repeat until no swaps available
  @scala.annotation.tailrec
  private def heapifyUp(i: Int): Unit =
    if i != 0 then
      val p = parent(i)

      if data(p) > data(i) then
        data.swap(p, i)
        heapifyUp(p)

  // 1. find minium child
  // 2. if child is smaller than parent, swap
  // 3. repeat until no swaps available
  private def heapifyDown(i: Int): Unit =
    if i >= treeHeight then ()
    else
      val l = left(i)
      val r = right(i)

      if l >= treeHeight then () // end of the heap
      else if data(l) > data(r) && data(i) > data(r) then // go right
        data.swap_(i, r)
        heapifyDown(r)
      else if data(r) > data(l) && data(i) > data(l) then // go left
        data.swap_(i, l)
        heapifyDown(l)
      else ()

  // Int is floored automatically: i.e. 5 / 2 = 2
  private def parent(i: Int): Int = (i - 1) / 2
  private def left(i: Int): Int = 2 * i + 1
  private def right(i: Int): Int = 2 * i + 2

object MinHeap:
  def empty[T: Ordering: ClassTag]: MinHeap[T] = new MinHeap[T]