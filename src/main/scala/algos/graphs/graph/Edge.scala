package algos.graphs.graph

import math.Ordering

type VertexIndex = Int

sealed trait Edge extends Product:
  def from: VertexIndex
  def to: VertexIndex

  def u: VertexIndex = from
  def v: VertexIndex = to

  def reversed: Edge

final case class UnweightedEdge(from: VertexIndex, to: VertexIndex) extends Edge:
  def reversed: UnweightedEdge = UnweightedEdge(to, from)

  override def toString: String = s"$from -> $to"

final case class WeightedEdge(from: VertexIndex, to: VertexIndex, weight: Double) extends Edge:
  def reversed: WeightedEdge = WeightedEdge(to, from, weight)

  override def toString: String = s"$from -$weight-> $to"
object WeightedEdge:
  given Ordering[WeightedEdge] = Ordering.by(_.weight)