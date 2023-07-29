package algos.graphs

import algos.graphs.search.*
import algos.*

type VertexIndex = Int

trait Edge extends Product:
  def from: VertexIndex
  def to: VertexIndex

  def u: VertexIndex = from
  def v: VertexIndex = to

  def reversed: Edge
  override def toString: String =
    from.toString + " -> " + to.toString

final case class UnweightedEdge(from: VertexIndex, to: VertexIndex) extends Edge:
  def reversed: UnweightedEdge = UnweightedEdge(to, from)

trait Graph[V, E <: Edge]:
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

  override def toString(): String =
    vertices.map(v => v.toString + " -> " + neighborsOf(v).mkString(", ")).mkString("\n")

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

  def addEdge(edge: UnweightedEdge): Option[UnweightedGraph[V]] =
    if !edges.isDefinedAt(edge.from) || !edges.isDefinedAt(edge.to) then None
    else Some(unsafeAddEdge(edge))

  def unsafeAddEdge(edge: UnweightedEdge): UnweightedGraph[V] =
    copy(
      edges =
        // undirected graph, we need to add from both sides
        edges
          .updated(edge.from, edges(edge.from) :+ edge)
          .updated(edge.to, edges(edge.to) :+ edge.reversed)
    )

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
