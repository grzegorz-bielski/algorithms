package algos.graphs

import scala.collection.mutable

// format: off
/**
 *  A graph representation. A graph with 4 vertices will have a square, 4x4 matrix in which:
 *   - A has 0 connections to A, 1 connection to B, 0 connections to C, 1 connection to D
 *   - B has 1 connection to A, 0 connections to B, 1 connection to C, 2 connections to D
 *   - // -
 * 
 *  Memory inefficient for sparse graphs.
 * 
 * [  A  B  C  D
 * A [0, 1, 0, 1],
 * B [1, 0, 1, 2],
 * C [0, 1, 0, 1],
 * D [1, 2, 1, 1]
 * ]
*/
opaque type AdjacencyMatrix = Array[Array[Int]]
// format: on
object AdjacencyMatrix:
  def fromArray(arr: Array[Array[Int]]): AdjacencyMatrix = arr

  extension (graph: AdjacencyMatrix)
    /** Breath first search with path reconstruction.
      */
    def bfs(source: Int, target: Int): List[Int] =
      // search for target
      @scala.annotation.tailrec
      def go(q: mutable.Queue[Int], seen: mutable.Set[Int], prev: Array[Option[Int]]): Array[Option[Int]] =
        if q.isEmpty then prev
        else
          // row with connections / adjacencies to other vertices
          val curr = q.dequeue()
          seen += curr

          if curr == target then prev
          else
            val adj = graph(curr)
            for
              vertex <- adj.indices
              // is non empty and not seen
              if adj(vertex) > 0 && !seen(vertex)
            yield
              seen += vertex
              prev(vertex) = Some(curr)
              q.enqueue(vertex) // check another row
            go(q, seen, prev)

      // search for target and produce a map from vertex to previous vertex
      val prev = go(
        mutable.Queue(source),
        mutable.Set.empty[Int],
        Array.fill(graph.length)(Option.empty[Int])
      )

      // reconstruct path
      if prev(target).isEmpty then List.empty[Int]
      else
        @scala.annotation.tailrec
        def go(curr: Int, xs: List[Int]): List[Int] =
          prev(curr) match
            case None       => xs
            case Some(next) => go(next, curr :: xs)

        val out = go(target, List.empty[Int])

        if out.nonEmpty then source +: out else out
