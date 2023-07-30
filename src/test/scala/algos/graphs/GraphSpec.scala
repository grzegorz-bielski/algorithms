package algos.graphs.graph

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

  val unweightedEdges = Vector(
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

  val weightedEdges = Vector[(String, String, Double)](
    ("Seattle", "Chicago", 1737),
    ("Seattle", "San Francisco", 678),
    ("San Francisco", "Riverside", 386),
    ("San Francisco", "Los Angeles", 348),
    ("Los Angeles", "Riverside", 50),
    ("Los Angeles", "Phoenix", 357),
    ("Riverside", "Phoenix", 307),
    ("Riverside", "Chicago", 1704),
    ("Phoenix", "Dallas", 887),
    ("Phoenix", "Houston", 1015),
    ("Dallas", "Chicago", 805),
    ("Dallas", "Atlanta", 721),
    ("Dallas", "Houston", 225),
    ("Houston", "Atlanta", 702),
    ("Houston", "Miami", 968),
    ("Atlanta", "Chicago", 588),
    ("Atlanta", "Washington", 543),
    ("Atlanta", "Miami", 604),
    ("Miami", "Washington", 923),
    ("Chicago", "Detroit", 238),
    ("Detroit", "Boston", 613),
    ("Detroit", "Washington", 396),
    ("Detroit", "New York", 482),
    ("Boston", "New York", 190),
    ("New York", "Philadelphia", 81),
    ("Philadelphia", "Washington", 123)
  )

  test("constructs unweighted graph properly"):
    val graph = UnweightedGraph.create(vertices, unweightedEdges)

    assert(graph.isDefined)

  test("constructs weighted graph properly"):
    val graph = WeightedGraph.create(vertices, weightedEdges)

    assert(graph.isDefined)

  test("can search the graph using generic search - dfs"):
    val path = UnweightedGraph
      .create(vertices, unweightedEdges)
      .flatMap: g =>
        import g.given
        GraphSearch.dfs[Id, String]("Boston", _ == "Miami")
      .map(_.toPath)

    path shouldBe Some(List("Boston", "Detroit", "Washington", "Miami"))

  test("can search the graph using generic search - bfs"):
    val path = UnweightedGraph
      .create(vertices, unweightedEdges)
      .flatMap: g =>
        import g.given
        GraphSearch.bfs[Id, String]("Boston", _ == "Miami")
      .map(_.toPath)

    path shouldBe Some(List("Boston", "Detroit", "Washington", "Miami"))
