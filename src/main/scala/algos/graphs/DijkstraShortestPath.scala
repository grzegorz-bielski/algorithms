package algos.graphs

import scala.annotation.tailrec

/** A naive implementation of Dijkstra's algorithm using immutable data structures.
  *
  * More generic and performant one available in `algos.graphs.graph.dijkstra`
  */
object DijkstraShortestPath:
  type Value = Double

  // DAG as HashMap of HashMaps using the adjacency list approach
  type WeightedGraph[T] = Map[T, Map[T, Value]]
  type Costs[T] = Map[T, Value]

  def apply[T](graph: WeightedGraph[T], source: T) =
    // would be faster to use a priority queue / minheap
    val initialCosts =
      graph.keys.foldLeft(Map[T, Value]()): (acc, t) =>
        acc + (if t == source then (t -> 0) else (t -> Double.PositiveInfinity))

    val initialVisited = Set[T]()
    val initialToVisit = graph.keys.toSet

    go(graph, initialToVisit, initialVisited, initialCosts)

  @tailrec
  def go[T](
      graph: WeightedGraph[T],
      toVisit: Set[T],
      visited: Set[T],
      costs: Costs[T]
  ): Costs[T] =
    if toVisit.isEmpty then costs
    else
      val node = (toVisit diff visited) minBy (costs.get)
      val toVisitPrim = toVisit - node
      val visitedPrim = visited + node
      val maybeCostsPrim = for
        neighbors <- graph get node
        cost <- costs get node
        costsPrim <- neighbors.keys
          .foldLeft[Option[Costs[T]]](Some(costs)): (acc, n) =>
            val maybeNewCost = neighbors.get(n).map(_ + cost)
            val isTheNewCostLower = maybeNewCost
              .flatMap(newCost => costs.get(n).map(_ > newCost))
              .getOrElse(false)

            if isTheNewCostLower then maybeNewCost.flatMap(newCost => acc.map(_ + (n -> newCost)))
            else acc
      yield costsPrim

      go(graph, toVisitPrim, visitedPrim, maybeCostsPrim.getOrElse(costs))
