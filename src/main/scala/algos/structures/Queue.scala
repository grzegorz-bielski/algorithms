package algos.structures

// invariant for mutable
trait MQueue[T]:
  def push(a: T): MQueue[T]
  def pop: (Option[T], MQueue[T])
  def peek: Option[T]

// covariant for immutable
trait IQueue[+T]:
  def push[A >: T](a: A): IQueue[A]
  def pop: (Option[T], IQueue[T])
  def peek: Option[T]

final class ImmutableQueue[T] private (underlying: LinkedList[T]) extends IQueue[T]:
  def push[A >: T](a: A): IQueue[A] =
    // poor performance, not a double-ended linked list, we don't have ref to the tail so need to traverse
    ImmutableQueue(underlying ++ LinkedList(a))
  def pop: (Option[T], IQueue[T]) =
    (underlying.head, ImmutableQueue(underlying.tail))
  def peek: Option[T] = underlying.head

object ImmutableQueue:
  def empty[T] = ImmutableQueue(LinkedList.empty[T])

final class MutableQueue[T] extends MQueue[T]:
  /** Inline, mutable, singly-linked, _double-ended_ linked list. (Not to be confused with double-linked!)
    */
  final private class Node(val value: T, var next: Option[Node])

  // ref to the first node in whole list
  private var head: Option[Node] = None
  // ref to the last node in whole list
  private var tail: Option[Node] = None

  private var length = 0

  // O(c)
  def push(a: T): MQueue[T] =
    val n = Some(Node(a, None))

    if length == 0 then
      // 1 element - tail and head refer to the same node
      tail = n
      head = n
    else
      // 2 and more elements - tail and head are different
      tail.foreach(_.next = n)
      tail = n

    length += 1

    this

  // O(c)
  def pop: (Option[T], MQueue[T]) =
    (
      head.map: h =>
        length -= 1
        head = h.next
        // free, handled by GC
        // h.next = None // removing refs from returned head
        // if length == 0 then tail = None

        h.value
      ,
      this
    )

  def peek: Option[T] = head.map(_.value)

object MutableQueue:
  def empty[T] = MutableQueue[T]
