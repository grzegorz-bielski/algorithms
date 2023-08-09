package algos.games

import scala.util.Random

type Move = Int

enum TTTPiece extends Piece:
  case X, O, E

  def next: TTTPiece = this match
    case X => O
    case O => X
    case E => E

final class TTTBoard(val positions: Vector[TTTPiece], val currentTurn: TTTPiece) extends Board[Move]:
  def move(pos: Move): TTTBoard =
    TTTBoard(positions.updated(pos, currentTurn), currentTurn.next)

  lazy val legalMoves: Vector[Move] = positions.zipWithIndex.collect:
    case (TTTPiece.E, i) => i

  lazy val won: Boolean =
    // TODO: make this generic?
    Vector(
      checkLine(0, 1, 2),
      checkLine(3, 4, 5),
      checkLine(6, 7, 8),
      checkLine(0, 3, 6),
      checkLine(1, 4, 7),
      checkLine(2, 5, 8),
      checkLine(0, 4, 8),
      checkLine(2, 4, 6)
    ).exists(identity)

  private def checkLine(ix: Int*): Boolean =
    val first = positions(ix.head)
    first != TTTPiece.E && ix.forall(positions(_) == first)

  def evaluate(piece: Piece): Double =
    currentState match
      case State.Win if currentTurn == piece => -1
      case State.Win if currentTurn != piece => 1 // win for maximazing player
      // could also return 0.5 for a draw
      case _ => 0

  override def toString(): String =
    val squareWidth = math.sqrt(TTTBoard.squares).toInt
    positions.grouped(squareWidth).map(_.mkString(" ")).mkString("\n")

object TTTBoard:
  val squares = 9
  def create(firstPlayer: TTTPiece = TTTPiece.X): TTTBoard =
    TTTBoard(Vector.fill(squares)(TTTPiece.E), firstPlayer)

object TTTGame:
  @main def playTTT =
    val player = TTTPiece.X

    @scala.annotation.tailrec
    def gameLoop(board: TTTBoard): Unit =
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
            // best move or random one on error (should not happen in TTT)
            case _ => MiniMax.findBestMove(board, maxDepth = 100).getOrElse(Random.nextInt(TTTBoard.squares))

          gameLoop(board.move(nextMove))

    gameLoop(TTTBoard.create(player))

  @scala.annotation.tailrec
  private def getPlayerMove(board: TTTBoard): Move =
    print("Your move: ")
    val playerInput = scala.io.StdIn.readLine()

    Option(playerInput).flatMap(_.toIntOption) match
      case Some(value) if board.legalMoves.contains(value) => value
      case _ =>
        println("Illegal move, try again.")
        getPlayerMove(board)
