package algos.games

import scala.util.control.TailCalls.*

// finding the best move in a two-player, zero-sum game

// After each attempt to maximize the gains of the maximizing player,
// minimax is called recursively to find the opponent`s reply that minimizes the maximizing player`s gains.

object MiniMax:
  enum Mode:
    case Min, Max

  enum Strategy:
    case Classic, AlphaBetaPruning

    def algorithm = this match
      case Classic          => classicMinimax
      case AlphaBetaPruning => alphaBetaMinimax

  def findBestMove(board: Board[Move], maxDepth: Int, strategy: Strategy = Strategy.Classic): Option[Move] =
    val originalPlayer = board.currentTurn
    val initialDepth = 1
    val algorithm = strategy.algorithm

    // first maximizing call, keeps track of initial move
    board.legalMoves
      .foldLeft((Double.MinValue, Option.empty[Move])): // None should be only returned in case of empty legal moves
        case ((bestEval, bestMove), move) =>
          val result = algorithm(Mode.Min, board.move(move), originalPlayer, initialDepth, maxDepth)
          if result > bestEval then (result, Some(move)) else (bestEval, bestMove)
      ._2

  private def classicMinimax(mode: Mode, board: Board[Move], originalPlayer: Piece, depth: Int, maxDepth: Int): Double =
    def go(mode: Mode, board: Board[Move], depth: Int): TailRec[Double] =
      if board.gameEnded || depth == maxDepth then done(board.evaluate(originalPlayer))
      else
        mode match
          case Mode.Min =>
            board.legalMoves.foldLeft(done(Double.MaxValue)): (acc, move) =>
              acc.flatMap(best => tailcall(go(Mode.Max, board.move(move), depth + 1)).map(math.min(best, _)))
          case Mode.Max =>
            board.legalMoves.foldLeft(done(Double.MinValue)): (acc, move) =>
              acc.flatMap(best => tailcall(go(Mode.Min, board.move(move), depth + 1)).map(math.max(best, _)))

    go(mode, board, depth).result

  // MiniMax extension with alpha beta pruning
  // keeps track of alpha and beta variables and decreases the search space
  // alpha - evaluation of the best maximizing move found so far
  // beta - evaluation of the best minimizing move found so far
  // if beta <= alpha, we can stop searching, since best minimizing move will not be greater than the best maximizing move
  private def alphaBetaMinimax(
      mode: Mode,
      board: Board[Move],
      originalPlayer: Piece,
      depth: Int,
      maxDepth: Int
  ): Double =
    def go(mode: Mode, board: Board[Move], depth: Int, alpha: Double, beta: Double): TailRec[Double] =
      if board.gameEnded || depth == maxDepth then done(board.evaluate(originalPlayer))
      else
        mode match
          case Mode.Min =>
            def findMin(moves: Vector[Move], depth: Int, beta: Double): TailRec[Double] =
              moves match
                case move +: rest =>
                  go(Mode.Max, board.move(move), depth + 1, alpha, beta)
                    .map(math.min(beta, _))
                    .flatMap: nextBeta =>
                      if nextBeta <= alpha then done(nextBeta)
                      else findMin(rest, depth, nextBeta)
                case _ => done(beta)

            findMin(board.legalMoves, depth, beta)

          case Mode.Max =>
            def findMax(moves: Vector[Move], depth: Int, alpha: Double): TailRec[Double] =
              moves match
                case move +: rest =>
                  go(Mode.Min, board.move(move), depth + 1, alpha, beta)
                    .map(math.max(alpha, _))
                    .flatMap: nextAlpha =>
                      if beta <= nextAlpha then done(nextAlpha)
                      else findMax(rest, depth, nextAlpha)
                case _ => done(alpha)

            findMax(board.legalMoves, depth, alpha)

    go(mode, board, depth = depth, alpha = Double.MinValue, beta = Double.MaxValue).result
