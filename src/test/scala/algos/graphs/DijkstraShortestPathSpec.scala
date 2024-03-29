package algos.graphs

import org.scalatest.*
import flatspec.*
import matchers.*

class DijkstraShortestPathSpec extends AnyFlatSpec, should.Matchers:
  case class Node(name: String)

  import DijkstraShortestPath.*

  // a directed graph
  val graph: WeightedGraph[Node] = Map(
    Node("start") -> Map(Node("a") -> 6, Node("b") -> 2),
    Node("a") -> Map(Node("end") -> 1),
    Node("b") -> Map(Node("a") -> 3, Node("end") -> 5),
    Node("end") -> Map()
  )

  "apply" should "return a Map with properly calculated costs" in:
    val costs = DijkstraShortestPath[Node](
      graph,
      Node("start")
    )

    costs should be(
      Map(
        Node("start") -> 0.0,
        Node("a") -> 5.0,
        Node("b") -> 2.0,
        Node("end") -> 6.0
      )
    )
