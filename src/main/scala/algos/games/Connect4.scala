package algos.games

import C4Board.*

@main def playC4 = CLIGame(C4Board.create(), C4Piece.R, 5).play()

enum C4Piece extends Piece:
  case R, B, E

  def next: C4Piece = this match
    case R => B
    case B => R
    case E => E

final case class C4Location(column: Int, row: Int)

final class C4Board(
    val positions: Vector[Vector[C4Piece]],
    val columnCount: Vector[Int],
    val currentTurn: C4Piece
) extends Board[Move]:
  def move(pos: Move): C4Board =
    C4Board.create(
      positions.updated(pos, positions(pos).updated(columnCount(pos), currentTurn)),
      currentTurn.next
    )

  def legalMoves: Vector[Move] =
    columnCount.zipWithIndex.collect:
      case (count, move) if count < rows => move

  def won: Boolean =
    segments.exists: segment =>
      countSegments(C4Piece.R, segment) == segmentLength ||
        countSegments(C4Piece.B, segment) == segmentLength

  def countSegments(piece: C4Piece, segment: Vector[C4Location]): Int =
    segment.count(loc => positions(loc.column)(loc.row) == piece)

  // check who has the advantage
  def evaluate(piece: Piece): Double =
    segments.foldLeft(0.0): (score, segment) =>
      val bLength = countSegments(C4Piece.B, segment)
      val rLength = countSegments(C4Piece.R, segment)

      if bLength > 0 || rLength > 0 then score
      else
        val best = if bLength > rLength then C4Piece.B else C4Piece.R
        val length = math.max(bLength, rLength)
        val nextScore = length match
          case 2 => score + 1
          case 3 => score + 100
          case 4 => score + 1000000
          case _ => score

        // negate for the other player
        if best == piece then nextScore else -nextScore

  override def toString(): String = positions.transpose.reverseIterator.map(_.mkString(" ")).mkString("\n")

object C4Board:
  val columns = 7
  val rows = 6
  val segmentLength = 4

  def create(positions: Vector[Vector[C4Piece]], piece: C4Piece): C4Board =
    C4Board(
      positions = positions,
      columnCount = positions.map(_.count(_ != C4Piece.E)),
      currentTurn = piece
    )

  def create(player: C4Piece = C4Piece.R): C4Board =
    C4Board(
      positions = Vector.fill(C4Board.columns)(Vector.fill(C4Board.rows)(C4Piece.E)),
      columnCount = Vector.fill(C4Board.columns)(0),
      currentTurn = player
    )

  val segments =
    val segments = (0 until segmentLength)

    val horizontal = for
      c <- 0 to columns - segmentLength
      r <- 0 until rows
    yield segments.map(dc => C4Location(c + dc, r))

    val vertical = for
      c <- 0 until columns
      r <- 0 to rows - segmentLength
    yield segments.map(dr => C4Location(c, r + dr))

    // diagonal from bottom left to top right
    val diagonal1 = for
      c <- 0 to columns - segmentLength
      r <- 0 to rows - segmentLength
    yield segments.map(d => C4Location(c + d, r + d))

    // diagonal from bottom right to top left
    val diagonal2 = for
      c <- columns - segmentLength to 0 by -1
      r <- segmentLength - 1 until rows
    yield segments.map(d => C4Location(c + d, r - d))

    Vector
      .concat(horizontal, vertical, diagonal1, diagonal2)
      .map(_.toVector)
