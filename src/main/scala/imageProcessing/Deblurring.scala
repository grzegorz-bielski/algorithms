package imageProcessing

object Deblurring {
  type Matrix[T] = Vector[Vector[T]]

  private def manhattanDistance(a: (Int, Int), b: (Int, Int)) = Math.abs(a._1 - b._1) + Math.abs(a._2 - b._2)

  def deblur(input: Matrix[Double], width: Int, height: Int, radius: Int): Matrix[Double] = {
    val size = width * height

    val coords =
      for {
        x <- input.indices
        y <- input(x).indices
      } yield (x, y)

    def coef =
      for {
        a <- coords
        b <- coords
        dist = manhattanDistance(a, b)
      } yield if (dist <= radius) 1d else 0d

    val coefSlice = coef.sliding(size, size).toVector
    val consts = coords.zipWithIndex.map {
      case ((x, y), i) => input(x)(y) * coefSlice(i).sum
    }.toArray

    val equationMatrix = coefSlice.zipWithIndex map {
      case (row, i) => row.toVector :+ consts(i)
    }

    val res = EquationSolver.backSubstitution(EquationSolver.gaussianElimination(equationMatrix, size))

    res.sliding(width, height).map(_.toVector).toVector
  }
  object EquationSolver {
    import scala.util.chaining._

    def gaussianElimination(A: Matrix[Double], size: Int) = {
      val rows = A.length
      val cols = A(0).length
      val n = size + 1
      val M = A.toArray.map(_.toArray)

      var row = 0
      (0 until cols - 1) foreach { col =>
        val pivot = (row + 1 until rows).foldLeft(row) { (prevPivot, i) =>
          if (Math.abs(M(i)(col)) > Math.abs(M(prevPivot)(col))) i else prevPivot
        }

        if (M(pivot)(col) == 0) {
          throw new IllegalArgumentException("The provided matrix is singular.")
        }

        if (col != pivot) {
          val temp = M(col)

          M(col) = M(pivot)
          M(pivot) = temp
        }

        (row + 1 until rows) foreach { i =>
          val scale = M(i)(col) / M(row)(col)

          (col + 1 until cols) foreach { j =>
            M(i)(j) -= (M(row)(j) * scale)
          }

          M(i)(col) = 0
        }

        row += 1
      }

      M
    }

    def backSubstitution(A: Array[Array[Double]]) = {
      val rows = A.length
      val cols = A(0).length

      (rows - 1 to 0 by -1).foldLeft(Array.ofDim[Double](rows)) { (S, i) =>
        val sum = (cols - 2 until i by -1).foldLeft(0d)((s, j) => s + S(j) * A(i)(j))

        S.updated(i, (A(i)(cols - 1) - sum) / A(i)(i))
      }
    }

  }

}
