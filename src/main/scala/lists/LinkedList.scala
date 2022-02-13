package lists

import scala.annotation.{targetName, tailrec}

val kek = List()

sealed abstract class LinkedList[+T]:
  def unsafeHead: T
  def head: Option[T]
  def tail: LinkedList[T]
  def isEmpty: Boolean

  @targetName("prepend")
  def ::[A >: T](elem: A): LinkedList[A] = LLCons(elem, this)

  def get(index: Int): Option[T]

object LinkedList:
  def empty[T]: LinkedList[T] = LLNil

  def apply[T](args: T*): LinkedList[T] =
    args.foldLeft(empty[T])((acc, a) => a :: acc)

  def unapplySeq[A](list: LinkedList[A]): Option[Seq[A]] =
    list match
      case LLNil              => Some(Seq.empty)
      case LLCons(head, tail) => unapplySeq(list.tail).map(head +: _)
end LinkedList

case object LLNil extends LinkedList[Nothing]:
  def unsafeHead: Nothing = throw new NoSuchElementException
  def head = None
  def tail = LLNil
  def isEmpty = true
  override def toString = "[]"

  def get(index: Int) = None
end LLNil

case class LLCons[+T](_head: T, tail: LinkedList[T]) extends LinkedList[T]:
  val unsafeHead = _head
  val head = Some(_head)

  override def toString =
    def go(remaining: LinkedList[T], result: String): String =
      remaining match
        case LLNil                           => result
        case LLCons(_, tail) if tail.isEmpty => s"$result${remaining.head}"
        case _                               => go(remaining.tail, s"$result${remaining.head}")
    s"[ ${go(this, "")} ]"

  def isEmpty = false

  def get(index: Int) =
    // O(min(N, index))
    @tailrec
    def go(i: Int, acc: LinkedList[T]): Option[T] =
      if i == index then acc.head
      else go(i + 1, acc.tail)

    go(0, this)

end LLCons

@main def LinkedListPlayground =
  val l = 1 :: 2 :: LLNil
  val l2 = LinkedList(1, 2, 3)
  val second = l2.get(1)
  println(second)
  val xd = l match
    case LinkedList(a, b, c) => (a, b, c)
    case _                   => (1, 2, 3)
