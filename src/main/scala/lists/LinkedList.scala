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
  def length: Int
  def reverse: LinkedList[T]
  @targetName("concat")
  def ++[A >: T](another: LinkedList[A]): LinkedList[A]

object LinkedList:
  def empty[T]: LinkedList[T] = LLNil

  def from[T](iterable: Iterable[T]) = 
    iterable.foldRight(empty[T])(_ :: _)

  def apply[T](args: T*): LinkedList[T] =
    args.foldRight(empty[T])(_ :: _)

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
  val length = 0
  val reverse = this

  @targetName("concat")
  def ++[A >: Nothing](another: LinkedList[A]) = another

end LLNil

case class LLCons[+T](_head: T, tail: LinkedList[T]) extends LinkedList[T]:
  val unsafeHead = _head
  val head = Some(_head)

  override def toString =
    @tailrec
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

  def length = 
     // O(N)
    @tailrec
    def go(i: Int, acc: LinkedList[T]): Int =
      if acc.isEmpty 
      then i
      else go(i + 1, acc.tail)

    go(0, this)

  def reverse =
    @tailrec
    def go(acc: LinkedList[T], remaining: LinkedList[T]): LinkedList[T] =
      remaining match 
        case LLNil => acc 
        case LLCons(head, tail) => go(head :: acc, tail)

    go(LLNil, this)

  @targetName("concat")
  def ++[A >: T](another: LinkedList[A]) = 
    // O(M) + O(M + N) -> O(M + N)
    // M - length of `another`
    // N - length of `this`
    @tailrec
    def go(acc: LinkedList[A], remaining: LinkedList[A]): LinkedList[A] =
      remaining match 
        case LLNil => acc 
        case LLCons(head, tail) => go(head :: acc, tail)

    go(this.reverse, another).reverse

end LLCons
