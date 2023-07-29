package algos.graphs

import org.scalatest.*, funsuite.*, matchers.*

class GraphSpec extends AnyFunSuite, should.Matchers:
  test("constructs graph properly"):
    val graph =
      UnweightedGraph.create(
        vertices = Vector(
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
        ),
        edges = Vector(
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
      )

    println(graph)

    assert(graph.isDefined)
