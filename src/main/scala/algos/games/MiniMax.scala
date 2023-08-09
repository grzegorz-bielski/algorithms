package algos.games

import scala.util.control.TailCalls.*

// finding the best move in a two-player, zero-sum game

// After each attempt to maximize the gains of the maximizing player,
// minimax is called recursively to find the opponent`s reply that minimizes the maximizing player`s gains.

object MiniMax:
  enum Mode:
    case Min, Max

  def findBestMove[Move](board: Board[Move], maxDepth: Int = 100): Option[Move] =
    val originalPlayer = board.currentTurn

    // format: off
    def findMove(initial: Double, fn: (Double, Double) => Double)(mode: Mode, board:  Board[Move], depth: Int): TailRec[Double] = 
      board.legalMoves.foldLeft(done(initial)): (acc, move) =>
        for
          best <- acc
          result <- tailcall(go(mode, board.move(move), depth + 1))
        yield fn(best, result)
    // format: on

    // find a move that yields a min possible evaluation
    def findMin = findMove(Double.MaxValue, math.min)
    // find a move that yields a max possible evaluation
    def findMax = findMove(Double.MinValue, math.max)

    def go(mode: Mode, board: Board[Move], depth: Int): TailRec[Double] =
      if board.gameEnded || depth == maxDepth then done(board.evaluate(originalPlayer))
      else
        mode match
          case Mode.Min => findMin(Mode.Max, board, depth)
          case Mode.Max => findMax(Mode.Min, board, depth)

    // first maximazing call, keeps track of initial move
    board.legalMoves
      .foldLeft(done((Double.MinValue, Option.empty[Move]))): (acc, move) =>
        for
          _acc <- acc
          (bestEval, bestMove) = _acc // source:future ?
          result <- tailcall(go(Mode.Min, board.move(move), 1))
        yield if result > bestEval then (result, Some(move)) else (bestEval, bestMove)
      .result
      ._2
