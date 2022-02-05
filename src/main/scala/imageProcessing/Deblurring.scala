package imageProcessing

import scala.util.chaining._

object Deblurring:
  type Matrix[T] = Vector[Vector[T]]

  def deblur(input: Matrix[Double], width: Int, height: Int, radius: Int): Matrix[Double] =
    val size = width * height

    val coords =
      for
        x <- input.indices
        y <- input(x).indices
      yield (x, y)

    val coef =
      for
        a <- coords
        b <- coords
        dist = manhattanDistance(a, b)
      yield if dist <= radius then 1d else 0d

    val coefSlice = coef.sliding(size, size).toArray
    val consts = coords.toArray.zipWithIndex.map { case ((x, y), i) => input(x)(y) * coefSlice(i).sum }
    val equationMatrix = coefSlice.zipWithIndex map { case (row, i) => row.toArray :+ consts(i) }

    val result = EquationSolver.solve(equationMatrix)

    result.sliding(width, height).toVector.map(_.toVector)

  private def manhattanDistance(a: (Int, Int), b: (Int, Int)) = Math.abs(a._1 - b._1) + Math.abs(a._2 - b._2)

  object EquationSolver:
    def solve(A: Array[Array[Double]]) =
      A pipe gaussianElimination pipe backSubstitution

    private def gaussianElimination(M: Array[Array[Double]]) =
      val rows = M.length
      val cols = M(0).length

      var row = 0
      (0 until cols - 1) foreach { col =>
        val pivot = (row + 1 until rows).foldLeft(row) { (prevPivot, i) =>
          if Math.abs(M(i)(col)) > Math.abs(M(prevPivot)(col)) then i else prevPivot
        }

        if M(pivot)(col) == 0 then
          throw new IllegalArgumentException("The provided matrix is singular.")

        if col != pivot then
          val temp = M(col)

          M(col) = M(pivot)
          M(pivot) = temp

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

    private def backSubstitution(M: Array[Array[Double]]) =
      val rows = M.length
      val cols = M(0).length

      (rows - 1 to 0 by -1).foldLeft(Array.ofDim[Double](rows)) { (S, i) =>
        val sum = (cols - 2 until i by -1).foldLeft(0d)((s, j) => s + S(j) * M(i)(j))

        S.updated(i, (M(i)(cols - 1) - sum) / M(i)(i))
      }

