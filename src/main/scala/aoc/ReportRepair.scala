package aoc

import scala.collection.mutable.Set

object ReportRepair extends App:
  def empty = Option.empty[Int]

  def partOneBruteForce =
    (for
      i <- readFile
      j <- readFile
    yield (i, j))
      .find({ case (a, b) => (a + b) == 2020 })
      .map(a => a._1 * a._2)

  // won't read more than necessary
  def parteOneCached =
    val set = Set[Int]()

    readFile.foldLeft(empty) { (z, n) =>
      z orElse {
        set add n
        set.find(_ + n == 2020).map(_ * n)
      }
    }

  def partTwoCached =
    val set = Set[Int]()

    readFile.foldLeft(empty)((z, n) =>
      z orElse {
        set add n
        set.foldLeft(empty)((z, a) =>
          z orElse {
            set.foldLeft(empty)((z, b) =>
              z orElse {
                Option.when((a + b + n) == 2020)(a * b * n)
              }
            )
          }
        )
      }
    )

  private def readFile =
    io.Source.fromResource("reportRepairInput.txt").getLines.map(_.toInt)

