package algos.games

trait Piece:
  /** Piece that follows after this turn
    */
  def next: Piece

enum State:
  case InProgress
  case Draw
  case Win

trait Board[Move]:
  def currentTurn: Piece

  def move(move: Move): Board[Move]
  def legalMoves: Vector[Move]
  def won: Boolean
  // check who has the advantage
  def evaluate(piece: Piece): Double

  final def currentState: State =
    (won, legalMoves.isEmpty) match
      case (true, _)     => State.Win
      case (false, true) => State.Draw
      case _             => State.InProgress

  final def inProgress = currentState == State.InProgress
  final def gameEnded = !inProgress
