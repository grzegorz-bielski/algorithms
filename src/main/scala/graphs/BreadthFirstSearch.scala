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

    def fromGraph(t: T): Option[Queue[T]] =
      graph.get(t).map(_.to(collection.immutable.Queue))

    @tailrec
    def go(searched: Map[T, Boolean])(queue: Queue[T]): Option[T] =
      if (queue.isEmpty) None
      else {
        val (t, nextQ) = queue.dequeue
        if (searched.get(t).isEmpty)
          if (predicate(t)) Some(t)
          else
            fromGraph(t).map(nextQ.enqueueAll) match {
              case None    => None
              case Some(q) => go(searched + (t -> true))(q)
            }
        else go(searched)(nextQ)
      }

    fromGraph(center).flatMap(go(Map()))
  }
}
