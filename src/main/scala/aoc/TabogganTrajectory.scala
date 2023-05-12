package aoc

object TabogganTrajectory:

  def partOne =
    countTrees(right = 3, down = 1)(using loadFile)

  def partTwo =
    implicit val grid = loadFile

    countTrees(right = 1, down = 1) *
      countTrees(right = 3, down = 1) *
      countTrees(right = 5, down = 1) *
      countTrees(right = 7, down = 1) *
      countTrees(right = 1, down = 2)

  private def countTrees(down: Int, right: Int)(using
      grid: Vector[Vector[GridSquare]]
  ) =
    val secondRow =
      if down > 1 then
        grid
          .drop(1)
          .zipWithIndex
          .filter { case (_, i) => (i + 1) % down == 0 }
          .map { case (e, _) => e }
      else grid.drop(1)

    secondRow
      .foldLeft(Result(0, (0, 0))): (z, line) =>
        val _y = z.pos._2 + right
        val y = if _y > (line.length - 1) then Math.abs(_y - line.length) else _y

        Result(
          if line(y).isTree then z.count + 1 else z.count,
          (z.pos._1 - 1, y)
        )
      .count

  private def loadFile =
    io.Source
      .fromResource("tabogganTrajectory.txt")
      .getLines
      .toVector
      .map(_.toVector.map(GridSquare.fromChar))

case class Result(count: Int, pos: (Int, Int))

abstract class GridSquare:
  def isTree: Boolean
object GridSquare:
  case object Tree extends GridSquare:
    def isTree: Boolean = true
  case object Empty extends GridSquare:
    def isTree: Boolean = false

  def fromChar(char: Char): GridSquare = char match
    case '.' => Empty
    case '#' => Tree
