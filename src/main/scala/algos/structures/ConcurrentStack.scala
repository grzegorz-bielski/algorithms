package algos.structures

import scala.language.unsafeNulls
import java.util.concurrent.atomic.AtomicReference

/** Non-blocking, lock-free, concurrent stack using Treiber's algorithm.
  */
final class ConcurrentStack[T] extends Stack[T]:
  /** Inline, mutable, singly-linked, _single-ended_ linked list
    */
  final private class Node(val value: T, var prev: Option[Node])

  // ref to the first node in whole list
  private var head: AtomicReference[Option[Node]] = AtomicReference(None)

  // O(c)
  def push(a: T): Stack[T] =
    val n = Some(Node(a, None))

    @scala.annotation.tailrec
    def go(): Unit =
      val old = head.get
      n.foreach(_.prev = old)
      if head.compareAndSet(old, n) then () else go()
    go()

    this

  // O(c)
  def pop: (Option[T], Stack[T]) =
    @scala.annotation.tailrec
    def go(): Option[T] =
      head.get match
        case None          => None
        case old @ Some(h) => if head.compareAndSet(old, h.prev) then Some(h.value) else go()
    (go(), this)

  def peek: Option[T] = head.get.map(_.value)

object ConcurrentStack:
  def empty[T] = ConcurrentStack[T]
