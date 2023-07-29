package algos.graphs.search

import scala.collection.mutable

trait Searchable[F[_], T]:
  extension (fa: F[T])
    def value: T
    def successors: Vector[F[T]]

object GraphSearch:
  def bfs[F[_], T](root: F[T], p: T => Boolean)(using Searchable[F, T]) = search(p):
    new SearchStructure[F, T]:
      val q = mutable.Queue(root)
      def take =
        val n = q.dequeue()
        (n, n.value)
      def isEmpty = q.isEmpty
      def addSuccessors(n: F[T]) =
        q ++= n.successors

  def dfs[F[_], T](root: F[T], p: T => Boolean)(using Searchable[F, T]) = search(p):
    new SearchStructure[F, T]:
      val s = mutable.Stack(root)
      def take =
        val n = s.pop()
        (n, n.value)
      def isEmpty = s.isEmpty
      def addSuccessors(n: F[T]) =
        s ++= n.successors

  // TODO: A*

  private def search[F[_], T](p: T => Boolean)(s: SearchStructure[F, T]): Option[T] =
    @scala.annotation.tailrec
    def go: Option[T] =
      if s.isEmpty then None
      else
        val (state, value) = s.take
        if p(value)
        then Some(value)
        else
          s.addSuccessors(state)
          go
    go

  private trait SearchStructure[F[_], T]:
    // pop / dequeue
    def take: (F[T], T)
    // push / append all successors
    def addSuccessors(n: F[T]): Unit
    def isEmpty: Boolean
