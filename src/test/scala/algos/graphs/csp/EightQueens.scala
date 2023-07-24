package algos.graphs.csp

import org.scalatest.*, funsuite.*, matchers.*

class EightQueensSpec extends AnyFunSuite, should.Matchers:
  test("Can place 8 queens on a chessboard without any of them attacking each other"):

    eightQueens shouldBe Right:
      Map(
        5 -> 1,
        1 -> 2,
        6 -> 4,
        2 -> 6,
        7 -> 7,
        3 -> 8,
        8 -> 5,
        4 -> 3
      )
