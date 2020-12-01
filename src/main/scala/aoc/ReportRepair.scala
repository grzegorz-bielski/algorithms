package aoc

object ReportRepair extends App {
  def apply() =
    (for {
      i <- readFile
      j <- readFile
    } yield (i, j))
      .find({ case (a, b) => (a + b) == 2020 })
      .map(a => a._1 * a._2)

  def readFile() =
    io.Source.fromResource("reportRepairInput.txt").getLines.map(_.toInt)

}
