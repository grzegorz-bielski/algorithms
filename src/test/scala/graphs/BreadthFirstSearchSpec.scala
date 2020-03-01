package graphs

import org.scalatest._

class BreadthFirstSearchSpec extends FlatSpec with Matchers {
  case class Node(name: String)
  val graph = Map(
    Node("me") -> List(Node("al"), Node("ba"), Node("ce")),
    Node("ba") -> List(Node("ja"), Node("pa")),
    Node("al") -> List(Node("pa")),
    Node("ce") -> List(Node("ta"), Node("jar")),
    Node("ja") -> List(),
    Node("pa") -> List(),
    Node("ta") -> List(),
    Node("jar") -> List()
  )

  "apply" should "return closest Some(node) that satisfies given predicate" in {
    BreadthFirstSearch[Node](
      graph,
      Node("me"),
      node => node.name.startsWith("p")
    ) should be(Some(Node("pa")))
  }

  "apply" should "not return None when there is no nodes that would satisfy the predicate" in {
    BreadthFirstSearch[Node](
      graph,
      Node("me"),
      node => node.name.startsWith("x")
    ) should be(None)
  }
}
