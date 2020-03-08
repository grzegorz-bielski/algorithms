package graphs

package graphs

import scala.annotation.tailrec

object DijkstraShorthestPath {
  type Value = Double
  type WeightedGraph[T] = Map[T, Map[T, Value]]
  type Costs[T] = Map[T, Value]
  type Parents[T] = Map[T, T]

  def apply[T](graph: WeightedGraph[T]): Unit = {
    val initialCosts =
      graph.keys.foldLeft(Map[T, Value]())((acc, t) =>
        acc + (t -> Double.PositiveInfinity)
      )

    val initialProcessed = Map[T, Boolean]()
    val intialParents = Map[T, T]()

    go[T](graph, initialCosts, intialParents, initialProcessed)
  }

  private def go[T](
      graph: WeightedGraph[T],
      costs: Costs[T],
      parents: Parents[T],
      processed: Map[T, Boolean]
  ): Costs[T] = {
    val maybeNode = findLowestCostNode[T](costs)

    if (maybeNode.isEmpty) costs
    else {
      val props = for {
        node <- maybeNode
        cost <- costs.get(node)
        neighbors <- graph.get(node)
        (costsPrim, parentsPrim) <- neighbors.keys
          .foldLeft[Option[(Costs[T], Parents[T])]](Some((costs, parents)))(
            (acc, n) => {
              val maybeNewCost = neighbors.get(n).map(_ + cost)
              val isTheCurrentCostLower = maybeNewCost
                .flatMap(newCost => costs.get(n).map(_ > newCost))
                .getOrElse(false)

              if (isTheCurrentCostLower)
                for {
                  newCost <- maybeNewCost
                  (c, p) <- acc
                } yield (c + (n -> newCost), p + (n -> node))
              else acc
            })
        val processedPrim = processed + (node -> true)
      } yield (costsPrim, parentsPrim, processedPrim)

      if (props.isEmpty) costs
      else {
        val (c, p, pr) = props.get
        go(graph, c, p, pr)
      }
    }
  }

  private def findLowestCostNode[T](costs: Costs[T]): Option[T] = ???
}
