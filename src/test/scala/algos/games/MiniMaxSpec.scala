package algos.games

import org.scalatest.*, funsuite.*, matchers.*

class MiniMaxSpec extends AnyFunSuite, should.Matchers:
  test("finds the best Tic Tac Toe move"):
    import TTTPiece.*

    val testCases = Vector(
      // trivial case
      TTTBoard(
        positions = Vector(
          // format: off
          X, O, X,
          X, E, O,
          E, E, O
          // format: on
        ),
        currentTurn = X
      ) -> 6,
      // block pos
      TTTBoard(
        positions = Vector(
          // format: off
          X, E, E,
          E, E, O,
          E, X, O
          // format: on
        ),
        currentTurn = X
      ) -> 2,
      TTTBoard(
        positions = Vector(
          // format: off
          X, E, E,
          E, E, O,
          O, X, E
          // format: on
        ),
        currentTurn = X
      ) -> 1
    )

    testCases.foreach: (board, expected) =>
      MiniMax.findBestMove(board) shouldBe Some(expected)
