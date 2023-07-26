package algos.graphs.csp

import org.scalatest.*, funsuite.*, matchers.*

class WordSearchSpec extends AnyFunSuite, should.Matchers:
  test("Can create a word search puzzle"):
    val locations = wordSearch.map(_._1)

    locations shouldBe Right:
      Map(
        "SALLY" -> Vector(
          GridLocation(5, 0),
          GridLocation(5, 1),
          GridLocation(5, 2),
          GridLocation(5, 3),
          GridLocation(5, 4)
        ),
        "SARAH" -> Vector(
          GridLocation(6, 0),
          GridLocation(6, 1),
          GridLocation(6, 2),
          GridLocation(6, 3),
          GridLocation(6, 4)
        ),
        "MATTHEW" -> Vector(
          GridLocation(0, 5),
          GridLocation(1, 5),
          GridLocation(2, 5),
          GridLocation(3, 5),
          GridLocation(4, 5),
          GridLocation(5, 5),
          GridLocation(6, 5)
        ),
        "JOE" -> Vector(
          GridLocation(7, 0),
          GridLocation(7, 1),
          GridLocation(7, 2)
        ),
        "MARY" -> Vector(
          GridLocation(7, 3),
          GridLocation(7, 4),
          GridLocation(7, 5),
          GridLocation(7, 6)
        )
      )
