package graphs

import scala.collection.immutable.Queue
import scala.annotation.tailrec

object BreadthFirstSearch {
  type Graph[T] = Map[T, List[T]]

  def apply[T](
      graph: Graph[T],
      center: T,
      predicate: T => Boolean
  ): Option[T] = {

    @tailrec
    def go(searched: Map[T, Boolean])(queue: Queue[T]): Option[T] =
      if (queue.isEmpty) None
      else {
        val (t, nextQ) = queue.dequeue
        if (searched.get(t).isEmpty)
          if (predicate(t)) Some(t)
          else
            from(graph, t).map(nextQ.enqueueAll) match {
              case None    => None
              case Some(q) => go(searched + (t -> true))(q)
            }
        else go(searched)(nextQ)
      }

    from(graph, center).flatMap(go(Map()))
  }

  def applyNoTailRec[T](
      graph: Graph[T],
      center: T,
      predicate: T => Boolean
  ): Option[T] = {

    def go(searched: Map[T, Boolean])(queue: Queue[T]): Option[T] =
      for {
        (t, nextQ) <- queue.dequeueOption
        res <- if (searched.get(t).isEmpty)
          if (predicate(t)) Some(t)
          else
            from(graph, t)
              .map(nextQ.enqueueAll)
              .flatMap(go(searched + (t -> true)))
        else go(searched)(nextQ)

      } yield res

    from(graph, center).flatMap(go(Map()))
  }

  private def from[T](graph: Graph[T], t: T) =
    graph.get(t).map(_.to(collection.immutable.Queue))
}
