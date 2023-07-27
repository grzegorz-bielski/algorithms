package algos.graphs.csp

// A word search puzzle generator
// Does not take into account overlapping words

@main def wordSearchProgram =
  wordSearch.fold(
    println,
    (locations, grid) =>
      println(s"Found solution:\n${grid.show}")
      println(
        s"Locations:\n${locations.map((word, loc) => s"$word: ${loc.map(l => l.row -> l.col).mkString(", ")}").mkString("\n")}"
      )
  )

def wordSearch =
  val words = Vector("MATTHEW", "JOE", "MARY", "SARAH", "SALLY")
  val columns = 9
  val rows = 9

  val domains = WordSearchDomains.fromWords(words, columns, rows)
  val constraint = WordSearchConstraint(words)

  CSP
    .search(domains, constraint)
    .map: locations =>
      locations -> locations.foldLeft(WordGrid.create(rows, columns, seed = 0L)):
        case (grid, (word, loc)) => grid.mark(word, loc)

type Word = String
type PossibleLocations = Vector[GridLocation]
type Domain = Vector[PossibleLocations]

final case class GridLocation(row: Row, col: Col)

final case class WordGrid(grid: Vector[Vector[Char]]):
  def show: String = grid.map(_.mkString(" ")).mkString("\n")

  def mark(word: String, locations: Vector[GridLocation]): WordGrid =
    WordGrid:
      word.indices.foldLeft(grid): (acc, i) =>
        (
          for
            loc <- locations.lift(i)
            row <- acc.lift(loc.row)
          yield acc.updated(loc.row, row.updated(loc.col, word(i)))
        ).getOrElse(acc)

object WordGrid:
  val AlphabetLength = 26
  val FirstLetter = 'A'

  def create(rows: Int, columns: Int, seed: Long) =
    val random = scala.util.Random(seed)
    val grid = Vector.tabulate(rows, columns): (row, col) =>
      (random.nextInt(AlphabetLength) + FirstLetter).toChar

    WordGrid(grid)

object WordSearchDomains:
  def fromWords(words: Vector[String], columns: Int, rows: Int): Map[Word, Domain] =
    words
      .groupMap(_.length)(word => word -> fromLength(word.length, columns, rows))
      .foldLeft(Map.empty)(_ ++ _._2.toMap)

  def fromLength(length: Int, columns: Int, rows: Int): Domain =
    inline def last(n: Col | Row) = n + length

    type Fill = PartialFunction[(Row, Col), Seq[GridLocation]]

    val fillRight: Fill =
      case (row, col) if last(col) <= columns =>
        (col until last(col)).map(GridLocation(row, _))

    val fillDiagonalRight: Fill =
      case (row, col) if last(col) <= columns && last(row) <= rows =>
        (col until last(col)).zipWithIndex.map((c, i) => GridLocation(row + i, c))

    val fillDown: Fill =
      case (row, col) if last(row) <= rows =>
        (row until last(row)).map(GridLocation(_, col))

    val fillDiagonalLeft: Fill =
      case (row, col) if last(row) <= rows && col - length >= 0 =>
        (row until last(row)).zipWithIndex.map((r, i) => GridLocation(r, col - i))

    (
      for
        row <- 0 until rows
        col <- 0 until columns
      yield Vector(fillRight, fillDiagonalRight, fillDown, fillDiagonalLeft).flatMap:
        _.andThen(_.toVector).applyOrElse((row, col), _ => Vector.empty)
    ).toVector

final case class WordSearchConstraint(words: Vector[String]) extends Constraint[Word, PossibleLocations]:
  def variables = words.toList

  def satisfied(assignment: Map[Word, PossibleLocations]): Boolean =
    lazy val allLocations = assignment.values.flatten
    lazy val isNotOverlapping = allLocations.size == allLocations.toSet.size
    lazy val nonEmpty = assignment.values.forall(_.nonEmpty)

    isNotOverlapping && nonEmpty
