package algos.structures

trait Queue[T]:
  def push(a: T): Queue[T]
  def pop: (Option[T], Queue[T])
  def peek: Option[T]

final class ImmutableQueue[T] private (underlying: LinkedList[T]) extends Queue[T]:
  def push(a: T): Queue[T] =
    // poor performance, not a double-ended linked list, we don't have ref to the tail so need to traverse
    ImmutableQueue(underlying ++ LinkedList(a))
  def pop: (Option[T], Queue[T]) =
    (underlying.head, ImmutableQueue(underlying.tail))
  def peek: Option[T] = underlying.head

object ImmutableQueue:
  def empty[T] = ImmutableQueue(LinkedList.empty[T])

final class MutableQueue[T] extends Queue[T]:
  /** Inline, mutable, singly-linked, _double-ended_ linked list. (Not to be confused with double-linked!)
    */
  final private class Node(val value: T, var next: Option[Node])

  // ref to the first node in whole list
  private var head: Option[Node] = None
  // ref to the last node in whole list
  private var tail: Option[Node] = None

  private var length = 0

  // O(c)
  def push(a: T): Queue[T] =
    val n = Some(Node(a, None))

    if length == 0 then
      // 1 element - tail and head refer to the same node
      tail = n
      head = n
    else
      // 2 and more elements - tail and head are different
      tail.foreach(_.next = n)
      tail = n

    length = length + 1

    this

  // O(c)
  def pop: (Option[T], Queue[T]) =
    (
      head.map: h =>
        length = length - 1
        head = h.next
        // free, not necessary
        h.next = None
        if length == 0 then tail = None

        h.value
      ,
      this
    )

  def peek: Option[T] = head.map(_.value)

object MutableQueue:
  def empty[T] = MutableQueue[T]
