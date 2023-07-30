package algos.graphs.graph

import scala.collection.mutable

/** A minimum spanning tree is a tree that connects every vertex in a weighted graph with the minimum total weight.
  */
opaque type MST = Vector[WeightedEdge]
extension (mst: MST)
  def totalWeight: Double =
    mst.view.map(_.weight).sum

  def showOn[V](graph: WeightedGraph[V]): String =
    mst.view
      .map: e =>
        for
          from <- graph.vertexAt(e.from)
          to <- graph.vertexAt(e.to)
        yield s"$from --${e.weight}--> $to"
      .collect { case Some(s) => s }
      .mkString("\n") + s"\nTotal Weight: $totalWeight"

extension [V](graph: WeightedGraph[V])
  // 1. Pick an arbitrary vertex to include in the MST
  // 2. Find the minimum-weight edge connecting the MST to the vertices not yet in the MST
  // 3. Add the vertex at the end of that edge to the MST
  // 4. Repeat steps 2 and 3 until all vertices are in the MST

  /** A Jarnik-Prim algorithm for finding the minimum spanning tree of a graph (MST).
    *
    * Greedy algorithm that builds the a tree from a starting vertex by adding the cheapest edge possible at each step.
    *
    * Will not work on directed or unconnected graphs.
    *
    * @param start
    *   The index of the vertex to start the search from.
    * @return
    *   a weight path representing the minimum spanning tree.
    */
  def mst(start: VertexIndex): MST =
    if start < 0 || start > graph.vertices.size - 1 then Vector.empty
    else
      val visited = mutable.Set(start)
      val minHeap = mutable.PriorityQueue.empty[WeightedEdge](using Ordering[WeightedEdge].reverse)

      def visitVertex(v: VertexIndex) =
        visited += v
        minHeap ++= graph.edgesOf(v).filterNot(e => visited.contains(e.to))

      visitVertex(start)

      @scala.annotation.tailrec
      def go(acc: MST): MST =
        if minHeap.isEmpty then acc
        else
          val nextEdge = minHeap.dequeue()
          if visited.contains(nextEdge.to)
          then go(acc)
          else
            visitVertex(nextEdge.to)
            go(acc :+ nextEdge)

      go(Vector.empty)
