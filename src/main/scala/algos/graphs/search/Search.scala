package algos.graphs.search

import scala.collection.mutable

trait Searchable[F[_], T]:
  extension (fa: F[T])
    def value: T
    def successors: Vector[F[T]]

trait WeightedSearchable[F[_], T] extends Searchable[F, T]:
  extension (fa: F[T])
    def cost(prev: Double): Double
    def heuristic: Double

object GraphSearch:
  def dfs[F[_], T](root: F[T], p: T => Boolean)(using Searchable[F, T]) = search(p):
    new SearchStructure[F, T]:
      val s = mutable.Stack(Node(root, None))
      def take = s.pop()
      def isEmpty = s.isEmpty
      def addSuccessors(n: Node[F[T]], _cost: Double) =
        s ++= n.current.successors.map(Node(_, Some(n)))

  def bfs[F[_], T](root: F[T], p: T => Boolean)(using Searchable[F, T]) = search(p):
    new SearchStructure[F, T]:
      val q = mutable.Queue(Node(root, None))
      def take = q.dequeue()
      def isEmpty = q.isEmpty
      def addSuccessors(n: Node[F[T]], _cost: Double) =
        q ++= n.current.successors.map(Node(_, Some(n)))

  def `A*`[F[_], T](root: F[T], p: T => Boolean)(using WeightedSearchable[F, T]) =
    weightedSearch(p):
      new SearchStructure[F, T]:
        // minHeap
        val q = mutable.PriorityQueue(Node(root, None))(using Ordering[Node[F[T]]].reverse)
        def take = q.dequeue()
        def isEmpty = q.isEmpty
        def addSuccessors(n: Node[F[T]], newCost: Double) =
          q ++= n.current.successors.map:
            Node(_, Some(n), Some(newCost), Some(n.current.heuristic))

  private def search[F[_], T](p: T => Boolean)(s: SearchStructure[F, T])(using Searchable[F, T]): Option[Node[F[T]]] =
    val visited = mutable.Set[T]()

    @scala.annotation.tailrec
    def go: Option[Node[F[T]]] =
      if s.isEmpty then None
      else
        val node = s.take
        val value = node.current.value

        if p(value) then Some(node)
        else if visited.contains(value) then go
        else
          visited += value
          s.addSuccessors(node, cost = 0)
          go
    go

  private def weightedSearch[F[_], T](p: T => Boolean)(s: SearchStructure[F, T])(using
      WeightedSearchable[F, T]
  ): Option[Node[F[T]]] =
    val visited = mutable.Map[T, Double]()

    @scala.annotation.tailrec
    def go: Option[Node[F[T]]] =
      if s.isEmpty then None
      else
        val node = s.take
        val value = node.current.value

        if p(value) then Some(node)
        else
          val newCost = node.current.cost(node.parent.flatMap(_.cost).getOrElse(0))

          if !visited.contains(value) || visited(value) > newCost then
            visited += (value -> newCost)
            s.addSuccessors(node, newCost)
            go
          else go
    go

  /** Inline immutable linked-list node for storing results of the search
    */
  final case class Node[T](
      current: T,
      parent: Option[Node[T]],
      cost: Option[Double] = None,
      heuristic: Option[Double] = None
  ):
    def toPath: List[T] =
      def go(n: Node[T], acc: List[T]): List[T] =
        n.parent match
          case None    => n.current :: acc
          case Some(p) => go(p, n.current :: acc)

      go(this, Nil)
  object Node:
    given [T]: Ordering[Node[T]] =
      Ordering.by: n =>
        n.cost.getOrElse(0.0) + n.heuristic.getOrElse(0.0)

  // TODO: split into WeightedSearchStructure and UnweightedSearchStructure ?
  private trait SearchStructure[F[_], T]:
    // pop / dequeue
    def take: Node[F[T]]
    // push / append all successors
    def addSuccessors(n: Node[F[T]], cost: Double): Unit
    def isEmpty: Boolean
