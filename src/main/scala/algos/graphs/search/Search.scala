package algos.graphs.search

import scala.collection.mutable

trait Searchable[F[_], T]:
  extension (fa: F[T])
    def value: T
    def successors: Vector[F[T]]

object GraphSearch:
  case class Node[T](current: T, parent: Option[Node[T]]):
    def toPath: List[T] =
      def go(n: Node[T], acc: List[T]): List[T] =
        n.parent match
          case None    => n.current :: acc
          case Some(p) => go(p, n.current :: acc)

      go(this, Nil)

  def bfs[F[_], T](root: F[T], p: T => Boolean)(using Searchable[F, T]) = search(p):
    new SearchStructure[F, T]:
      val q = mutable.Queue(Node(root, None))
      def take = q.dequeue()
      def isEmpty = q.isEmpty
      def addSuccessors(n: Node[F[T]]) =
        q ++= n.current.successors.map(Node(_, Some(n)))

  def dfs[F[_], T](root: F[T], p: T => Boolean)(using Searchable[F, T]) = search(p):
    new SearchStructure[F, T]:
      val s = mutable.Stack(Node(root, None))
      def take = s.pop()
      def isEmpty = s.isEmpty
      def addSuccessors(n: Node[F[T]]) =
        s ++= n.current.successors.map(Node(_, Some(n)))

  // TODO: A*

  private def search[F[_], T](p: T => Boolean)(s: SearchStructure[F, T])(using Searchable[F, T]): Option[Node[F[T]]] =
    @scala.annotation.tailrec
    def go: Option[Node[F[T]]] =
      if s.isEmpty then None
      else
        val node = s.take
        val value = node.current.value

        if p(value) then Some(node)
        else if s.visited.contains(value) then go
        else
          s.visited += value
          s.addSuccessors(node)
          go
    go

  private trait SearchStructure[F[_], T]:
    // pop / dequeue
    def take: Node[F[T]]
    // push / append all successors
    def addSuccessors(n: Node[F[T]]): Unit
    def isEmpty: Boolean

    val visited = mutable.Set[T]()
