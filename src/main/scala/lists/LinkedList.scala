package lists

import scala.annotation.{targetName, tailrec}

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
  def removeAt(index: Int): LinkedList[T]
  def map[A](fn: T => A): LinkedList[A]
  def flatMap[A](fn: T => LinkedList[A]): LinkedList[A]
  def filter(fn: T => Boolean): LinkedList[T]

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

  def removeAt(index: Int): LinkedList[Nothing] = LLNil
  def map[A](fn: Nothing => A): LinkedList[A] = LLNil
  def flatMap[A](fn: Nothing => LinkedList[A]): LinkedList[A] = LLNil
  def filter(fn: Nothing => Boolean): LinkedList[Nothing] = LLNil

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
        case LLNil              => acc
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
        case LLNil              => acc
        case LLCons(head, tail) => go(head :: acc, tail)

    go(this.reverse, another).reverse

  def removeAt(index: Int): LinkedList[T] =
    @tailrec
    def go(i: Int, toLeft: LinkedList[T], toRight: LinkedList[T]): LinkedList[T] =
      toRight match
        case LLNil                         => toLeft.reverse
        case LLCons(_, tail) if i == index => toLeft.reverse ++ tail
        case LLCons(head, tail)            => go(i + 1, head :: toLeft, tail)

    go(0, LLNil, this)

  def flatMap[A](fn: T => LinkedList[A]): LinkedList[A] =
    // Z = sum of all lengths of fn(x)
    // O(Z^2) (!)
    @tailrec
    def go(applied: LinkedList[A], rest: LinkedList[T]): LinkedList[A] =
      rest match
        case LLNil              => applied.reverse
        case LLCons(head, tail) => go(fn(head).reverse ++ applied, tail)

    go(LLNil, this)

  def map[A](fn: T => A): LinkedList[A] =
    flatMap(fn.andThen(LinkedList(_)))

  def filter(fn: T => Boolean): LinkedList[T] =
    @tailrec
    def go(acc: LinkedList[T], rest: LinkedList[T]): LinkedList[T] =
      rest match
        case LLNil => acc.reverse
        case LLCons(head, tail) =>
          if fn(head) then go(head :: acc, rest.tail)
          else go(acc, rest.tail)

    go(LLNil, this)
