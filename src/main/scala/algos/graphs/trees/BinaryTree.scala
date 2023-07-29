package algos.graphs.trees

import algos.graphs.search.*

import scala.util.control.TailCalls.*
import scala.collection.mutable

import BinaryTree.*

trait BinaryTree[T]:
  def value: T
  def left: Option[BinaryTree[T]]
  def right: Option[BinaryTree[T]]

  type Search[A] = (A => Boolean) => Option[A]

  given Searchable[BinaryTree, T] with
    extension (fa: BinaryTree[T])
      def value: T = fa.value
      def successors: Vector[BinaryTree[T]] =
        (fa.left ++ fa.right).toVector

  val bfs: Search[T] = GraphSearch.bfs(this, _)
  val dfs: Search[T] = GraphSearch.dfs(this, _)

  def compare(other: BinaryTree[T]): Boolean =
    def go(a: Option[BinaryTree[T]], b: Option[BinaryTree[T]]): TailRec[Boolean] =
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
