package algos.graphs.trees

import scala.util.control.TailCalls.*
import scala.collection.mutable

import BinaryTree.*

trait BinaryTree[+T]:
  def value: T
  def left: Option[BinaryTree[T]]
  def right: Option[BinaryTree[T]]

  private val root = this

  private type Nodes = IterableOnce[BinaryTree[T]]
  private trait Structure:
    // pop / dequeue
    def take: BinaryTree[T]
    // push / append all
    def addAll(xs: Nodes): Unit
    def isEmpty: Boolean

  type Search[A] = (A => Boolean) => Option[A]

  val bfs: Search[T] = search:
    new:
      val q = mutable.Queue(root)
      def take = q.dequeue()
      def isEmpty = q.isEmpty
      def addAll(xs: Nodes) = q ++= xs

  val dfs: Search[T] = search:
    new:
      val s = mutable.Stack(root)
      def take = s.pop()
      def isEmpty = s.isEmpty
      def addAll(xs: Nodes) = s ++= xs

  // TODO: A*

  private def search(xs: Structure)(p: T => Boolean): Option[T] =
    @scala.annotation.tailrec
    def go: Option[T] =
      if xs.isEmpty then None
      else
        val n = xs.take
        if p(n.value)
        then Some(n.value)
        else
          xs.addAll(n.left ++ n.right)
          go
    go

  def compare[A >: T](other: BinaryTree[A]): Boolean =
    def go(a: Option[BinaryTree[A]], b: Option[BinaryTree[A]]): TailRec[Boolean] =
      (a, b) match
        case (None, None)                             => done(true)
        case (Some(_), None) | (None, Some(_))        => done(false)
        case (Some(x), Some(y)) if x.value != y.value => done(false)
        case (Some(x), Some(y)) =>
          for
            left <- tailcall(go(x.left, y.left))
            right <- tailcall(go(x.right, y.right))
          yield left && right

    go(Some(this), Some(other)).result

  // Depth-first traversal, preserves the shape of the tree
  def walk(traversal: Traversal): List[T] =
    val visited = collection.mutable.ArrayBuffer.empty[T]

    val nothing = (_: BinaryTree[T]) => ()
    val addOne = (n: BinaryTree[T]) => visited += n.value
    val (pre, in, post) = traversal match
      case Traversal.PreOrder  => (addOne, nothing, nothing)
      case Traversal.InOrder   => (nothing, addOne, nothing)
      case Traversal.PostOrder => (nothing, nothing, addOne)

    def go(node: Option[BinaryTree[T]]): TailRec[Unit] = node.fold(done(())): n =>
      pre(n)
      for
        _ <- tailcall(go(n.left))
        _ = in(n)
        _ <- tailcall(go(n.right))
        _ = post(n)
      yield ()

    go(Some(this)).result
    visited.toList

object BinaryTree:
  enum Traversal:
    case PreOrder, InOrder, PostOrder
