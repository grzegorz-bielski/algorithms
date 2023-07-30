package algos.graphs.graph

package dijkstra

import math.Ordering
import scala.collection.mutable

/** @param costs
  *   a cost to every vertex from the root
  * @param pathMap
  *   a map from vertex to the next edge in the path
  */
final case class SearchResult(costs: Map[VertexIndex, Double], pathMap: Map[VertexIndex, WeightedEdge]):
  def path(from: VertexIndex, to: VertexIndex): Vector[WeightedEdge] =
    @scala.annotation.tailrec
    def go(acc: List[WeightedEdge], to: VertexIndex): List[WeightedEdge] =
      pathMap.get(to) match
        case None => List.empty
        case Some(edge) =>
          if edge.from == from then edge +: acc
          else go(edge +: acc, edge.from)

    go(List.empty, to).toVector

final case class Node(vertex: VertexIndex, cost: Double)
object Node:
  given Ordering[Node] = Ordering.by(_.cost)

extension [V](graph: WeightedGraph[V])

  /** A Dijkstra algorithm for finding the shortest path in a graph using a MinHeap / Priority Queue. A special case of
    * A*.
    *
    * Finds shortest-path tree from a specified source to all possible goals.
    *
    * With heuristics and restriction to single destination it will become the A* algorithm.
    *
    * Doesn't work on graphs with negative weights.
    */
  def dijkstra(root: V): Option[SearchResult] =
    graph
      .indexOf(root)
      .map: initial =>
        val costs = mutable.Map[VertexIndex, Double](initial -> 0)
        val visited = mutable.Set[VertexIndex](initial)
        val pathToMap = mutable.Map[VertexIndex, WeightedEdge]()
        val toVisit = mutable.PriorityQueue(Node(initial, 0))(using Ordering[Node].reverse) // minHeap

        @scala.annotation.tailrec
        def go(): Unit =
          if toVisit.isEmpty then ()
          else
            val Node(vertex, _) = toVisit.dequeue()
            val fromCost = costs(vertex) // should be defined

            graph
              .edgesOf(vertex)
              .foreach: edge =>
                val oldCost = costs.get(edge.to)
                val newCost = fromCost + edge.weight

                if !visited.contains(edge.to) || oldCost.exists(_ > newCost) then
                  visited += edge.to
                  costs += edge.to -> newCost
                  pathToMap += edge.to -> edge
                  toVisit += Node(edge.to, newCost)
            go()
        go()

        SearchResult(costs.toMap, pathToMap.toMap)

  def path(from: V, to: V): Vector[(V, V, Double)] =
    for
      searchResult <- graph.dijkstra(from).toVector
      fromIndex <- graph.indexOf(from).toVector
      toIndex <- graph.indexOf(to).toVector
      edge <- searchResult.path(fromIndex, toIndex)
      from <- graph.vertexAt(edge.from).toVector
      to <- graph.vertexAt(edge.to).toVector
    yield (from, to, edge.weight)
