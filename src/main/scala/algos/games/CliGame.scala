package algos.games

import scala.util.Random

type Move = Int

final class CLIGame(initialBoard: Board[Move], player: Piece, miniMaxDepth: Int):
  def play(): Unit =
    @scala.annotation.tailrec
    def gameLoop(board: Board[Move]): Unit =
      board.currentState match
        case State.Draw =>
          println("Draw")
          println(board)
        case State.Win =>
          val winner = board.currentTurn.next // prev turn won
          println(s"Winner: $winner!")
          println(board)
        case State.InProgress =>
          println(s"Turn: ${board.currentTurn}")
          println(board)
          val nextMove = board.currentTurn match
            case `player` => getPlayerMove(board)
            // best move or random one on error (should not happen)
            case _ => MiniMax.findBestMove(board, miniMaxDepth).getOrElse(Random.nextInt(board.legalMoves.max))

          gameLoop(board.move(nextMove))

    gameLoop(initialBoard)

  @scala.annotation.tailrec
  private def getPlayerMove(board: Board[Move]): Move =
    print("Your move: ")
    val playerInput = scala.io.StdIn.readLine()

    Option(playerInput).flatMap(_.toIntOption) match
      case Some(value) if board.legalMoves.contains(value) => value
      case _ =>
        println("Illegal move, try again.")
        getPlayerMove(board)
