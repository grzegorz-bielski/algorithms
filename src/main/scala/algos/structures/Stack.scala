package algos.structures

trait Stack[T]:
  def push(a: T): Stack[T]
  def pop: (Option[T], Stack[T])
  def peek: Option[T]

final class MutableStack[T] extends Stack[T]:
  /** Inline, mutable, singly-linked, _single-ended_ linked list
    */
  final private class Node(val value: T, var prev: Option[Node])

  // ref to the first node in whole list
  private var head: Option[Node] = None

  private var length = 0

  // O(c)
  def push(a: T): Stack[T] =
    val n = Some(Node(a, None))

    if length == 0 then head = n
    else
      n.foreach(_.prev = head)
      head = n

    length = length + 1

    this

  // O(c)
  def pop: (Option[T], Stack[T]) =
    (
      head.map: h =>
        length = length - 1
        head = h.prev
        // free handled by GC
        // h.prev = None // removing refs from returned head

        h.value
      ,
      this
    )

  def peek: Option[T] = head.map(_.value)

object MutableStack:
  def empty[T] = MutableStack[T]
