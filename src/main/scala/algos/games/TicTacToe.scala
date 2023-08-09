package algos.games

@main def playTTT = CLIGame(TTTBoard.create(), TTTPiece.X, 9).play()

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
