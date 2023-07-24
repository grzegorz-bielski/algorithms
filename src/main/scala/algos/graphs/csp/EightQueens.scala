package algos.graphs.csp

// how 8 queens could be placed ona chessboard without any of them attacking each other

@main
def eightQueensProgram =
  println:
    eightQueens.fold(
      e => s"Failed with: $e",
      s => s"Found solution: $s"
    )

type Col = Int
type Row = Int

def eightQueens =
  val cols = (1 to 8)
  val rows = (1 to 8)
  val domains =
    rows.map(_ -> cols.toVector).toMap

  object QueensConstraint extends Constraint[Col, Row]:
    def variables = cols.toList
    def satisfied(assignment: Map[Col, Row]): Boolean =
      assignment.forall: (col, row) =>
        cols
          .drop(col)
          .forall: otherCol =>
            assignment
              .get(otherCol)
              .map: otherRow =>
                val sameRow = row == otherRow
                lazy val sameDiagonal =
                  math.abs(row - otherRow) == math.abs(col - otherCol)

                !sameRow && !sameDiagonal
              .getOrElse(true)

  CSP
    .create(domains, QueensConstraint)
    .flatMap(_.search.toRight("No solution found"))
