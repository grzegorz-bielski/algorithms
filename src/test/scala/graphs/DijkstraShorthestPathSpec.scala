package graphs

import org.scalatest._

class DijkstraShorthestPathSpec extends FlatSpec with Matchers {
  case class Node(name: String)

  // a directet graph where `start` is the source node
  val graph = Map(
    Node("start") -> Map(Node("a") -> 6, Node("b") -> 2),
    Node("a") -> Map(Node("end") -> 1),
    Node("b") -> Map(Node("a") -> 3, Node("end") -> 5),
    Node("end") -> Map()
  )

  "apply" should "return" in {}
}
