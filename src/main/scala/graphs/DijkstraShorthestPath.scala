package graphs

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object DijkstraShorthestPath {
  type Value = Double
  type WeightedGraph[T] = Map[T, Map[T, Value]]
  type Costs[T] = Map[T, Value]

  def apply[T](graph: WeightedGraph[T], source: T) = {
    val initialCosts =
      graph.keys.foldLeft(Map[T, Value]())((acc, t) =>
        (acc + (if (t == source) (t -> 0) else (t -> Double.PositiveInfinity)))
      )
    val initialVisited = Set[T]()
    val initialToVisit = graph.keys.toSet

    go(graph, initialToVisit, initialVisited, initialCosts)
  }

  @tailrec
  def go[T](
      graph: WeightedGraph[T],
      toVisit: Set[T],
      visited: Set[T],
      costs: Costs[T]
  ): Costs[T] =
    if (toVisit.isEmpty) costs
    else {
      val node = (toVisit diff visited) minBy (costs.get)
      val toVisitPrim = toVisit - node
      val visitedPrim = visited + node
      val maybeCostsPrim = for {
        neighbors <- graph get node
        cost <- costs get node
        costsPrim <- neighbors.keys
          .foldLeft[Option[Costs[T]]](Some(costs))((acc, n) => {
            val maybeNewCost = neighbors.get(n).map(_ + cost)
            val isTheNewCostLower = maybeNewCost
              .flatMap(newCost => costs.get(n).map(_ > newCost))
              .getOrElse(false)

            if (isTheNewCostLower)
              maybeNewCost.flatMap(newCost => acc.map(c => c + (n -> newCost)))
            else acc
          })
      } yield costsPrim

      if (maybeCostsPrim.isEmpty) go(graph, toVisitPrim, visitedPrim, costs)
      else go(graph, toVisitPrim, visitedPrim, maybeCostsPrim.get)
    }
}
