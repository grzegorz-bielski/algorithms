package algos.graphs.graph

import algos.graphs.search.*
import algos.*

sealed trait Graph[V, E <: Edge]:
  def vertices: Vector[V]
  // adjacency list
  def edges: Vector[Vector[E]]

  def vertexAt(index: VertexIndex): Option[V] = vertices.lift(index)

  def indexOf(v: V): Option[VertexIndex] = vertices.indexOf(v) match
    case -1 => None
    case i  => Some(i)

  def neighborsOf(index: VertexIndex): Vector[V] =
    for
      edges <- edges.lift(index).toVector
      edge <- edges
      vertices <- vertexAt(edge.to)
    yield vertices

  def neighborsOf(vertex: V): Vector[V] =
    indexOf(vertex).toVector.flatMap(neighborsOf)

  def edgesOf(index: VertexIndex): Vector[E] =
    edges.lift(index).toVector.flatten

  def edgesOf(vertex: V): Vector[E] =
    indexOf(vertex).toVector.flatMap(edgesOf)

  def addEdge[G[I, _] <: Graph[_, _], I <: E](edge: I): Option[G[V, I]] =
    if !edges.isDefinedAt(edge.from) || !edges.isDefinedAt(edge.to) then None
    else Some(unsafeAddEdge(edge))

  def unsafeAddEdge[G[_, _] <: Graph[_, _], I <: E](edge: I): G[V, I] =
    // TODO: fix variance checks...
    withEdges:
      // undirected graph, we need to add from both sides
      edges
        .updated(edge.from, edges(edge.from) :+ edge)
        .updated(
          edge.to,
          edges(edge.to) :+ edge.reversed.asInstanceOf[E]
        )
    .asInstanceOf[G[V, I]]

  protected def withEdges(edges: Vector[Vector[E]]): Graph[V, E]

final case class UnweightedGraph[V](vertices: Vector[V], edges: Vector[Vector[UnweightedEdge]])
    extends Graph[V, UnweightedEdge]:
  def addVertex(v: V): (UnweightedGraph[V], VertexIndex) =
    (UnweightedGraph(vertices :+ v, edges :+ Vector.empty), vertices.size - 1)

  def addEdge(from: V, to: V): Option[UnweightedGraph[V]] =
    for
      fromIndex <- indexOf(from)
      toIndex <- indexOf(to)
      edge <- addEdge(fromIndex, toIndex)
    yield edge

  def addEdge(from: VertexIndex, to: VertexIndex): Option[UnweightedGraph[V]] =
    addEdge(UnweightedEdge(from, to))

  protected def withEdges(edges: Vector[Vector[UnweightedEdge]]): UnweightedGraph[V] =
    copy(edges = edges)

  override def toString(): String =
    vertices.map(v => s"$v -> ${neighborsOf(v).mkString(", ")}").mkString("\n")

  given Searchable[Id, V] with
    extension (fa: Id[V])
      def value: V = fa
      def successors: Vector[Id[V]] = neighborsOf(value)

object UnweightedGraph:
  def create[V](vertices: Vector[V], edges: Vector[(V, V)]): Option[UnweightedGraph[V]] =
    val init: Option[UnweightedGraph[V]] =
      Some(UnweightedGraph(vertices, vertices.map(_ => Vector.empty[UnweightedEdge])))

    edges.foldLeft(init):
      case (Some(graph), (from, to)) => graph.addEdge(from, to)
      case (None, _)                 => None // short circuit with cats foldM & Left ?

final case class WeightedGraph[V](vertices: Vector[V], edges: Vector[Vector[WeightedEdge]])
    extends Graph[V, WeightedEdge]:
  def addVertex(v: V): (WeightedGraph[V], VertexIndex) =
    (WeightedGraph(vertices :+ v, edges :+ Vector.empty), vertices.size - 1)

  def addEdge(from: V, to: V, weight: Double): Option[WeightedGraph[V]] =
    for
      fromIndex <- indexOf(from)
      toIndex <- indexOf(to)
      edge <- addEdge(fromIndex, toIndex, weight)
    yield edge

  def addEdge(from: VertexIndex, to: VertexIndex, weight: Double): Option[WeightedGraph[V]] =
    addEdge(WeightedEdge(from, to, weight))

  protected def withEdges(edges: Vector[Vector[WeightedEdge]]): WeightedGraph[V] =
    copy(edges = edges)

  override def toString(): String =
    vertices
      .map: v =>
        edgesOf(v).map(e => s"$v -> ${e.to} (${e.weight})").mkString(", ")
      .mkString("\n")

  // given WeightedSearchable[Id, V] with
  //   extension (fa: Id[V])
  //     def value: V = fa
  //     def successors: Vector[Id[V]] = neighborsOf(value)

  //     def cost(prev: Double): Double = prev + weight

  //     def heuristic: Double = 1 // TODO: euclidean distance / manhattan distance

object WeightedGraph:
  def create[V](vertices: Vector[V], edges: Vector[(V, V, Double)]): Option[WeightedGraph[V]] =
    val init: Option[WeightedGraph[V]] =
      Some(WeightedGraph(vertices, vertices.map(_ => Vector.empty[WeightedEdge])))

    edges.foldLeft(init):
      case (Some(graph), (from, to, weight)) => graph.addEdge(from, to, weight)
      case (None, _)                         => None // short circuit with cats foldM & Left ?
