package algos.graphs

import org.scalatest.*, funsuite.*, matchers.*
import algos.graphs.search.*
import algos.*

class GraphSpec extends AnyFunSuite, should.Matchers:
  val vertices = Vector(
    "Seattle",
    "San Francisco",
    "Los Angeles",
    "Riverside",
    "Phoenix",
    "Chicago",
    "Boston",
    "New York",
    "Atlanta",
    "Miami",
    "Dallas",
    "Houston",
    "Detroit",
    "Philadelphia",
    "Washington"
  )
  val edges = Vector(
    ("Seattle", "Chicago"),
    ("Seattle", "San Francisco"),
    ("San Francisco", "Riverside"),
    ("San Francisco", "Los Angeles"),
    ("Los Angeles", "Riverside"),
    ("Los Angeles", "Phoenix"),
    ("Riverside", "Phoenix"),
    ("Riverside", "Chicago"),
    ("Phoenix", "Dallas"),
    ("Phoenix", "Houston"),
    ("Dallas", "Chicago"),
    ("Dallas", "Atlanta"),
    ("Dallas", "Houston"),
    ("Houston", "Atlanta"),
    ("Houston", "Miami"),
    ("Atlanta", "Chicago"),
    ("Atlanta", "Washington"),
    ("Atlanta", "Miami"),
    ("Miami", "Washington"),
    ("Chicago", "Detroit"),
    ("Detroit", "Boston"),
    ("Detroit", "Washington"),
    ("Detroit", "New York"),
    ("Boston", "New York"),
    ("New York", "Philadelphia"),
    ("Philadelphia", "Washington")
  )

  test("constructs graph properly"):
    val graph = UnweightedGraph.create(vertices, edges)

    assert(graph.isDefined)

  test("can search the graph using generic search"):
    val path = UnweightedGraph
      .create(vertices, edges)
      .flatMap: g =>
        import g.given
        GraphSearch.dfs[Id, String]("Boston", _ == "Miami")
      .map(_.toPath)

    path shouldBe Some(List("Boston", "Detroit", "Washington", "Miami"))
