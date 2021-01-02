package imageProcessing

object Deblurring {
  // import breeze.linalg._
  // import breeze.linalg.functions.manhattanDistance

  // read data (not handled)
  //  \/
  // input matrix
  //        -> coef matrix
  //           1. (1, 1, 1, 0, 0, 0, 0, 0)
  //           2. (1, 1, 1, 0, 0, 0, 0, 0)
  //           ...
  //           9. (0, 1, 1, 0, 0, 0, 0, 0)
  //        -> consts vector
  //           (19 * 3, 14 * 4, ...) for 3 x 3 and 1 blur radius
  //        -> lu(coef matrix, consts vector)
  // solution vector
  //  \/
  // solution matrix
  type Matrix[T] = Vector[Vector[T]]

  def deblur(input: Matrix[Double], width: Int, height: Int, radius: Int): Matrix[Double] = {
    val size = width * height
    def coords = input.keys.iterator.map { case (x, y) => DenseVector(x, y) }
    def coef =
      for {
        a <- coords
        b <- coords
        dist = manhattanDistance(a, b)
      } yield if (dist <= radius) 1d else 0d

    val coefSlice = coef.sliding(size, size).toVector
    val consts = input.keys.iterator.zipWithIndex.map {
      case ((x, y), i) => input(x, y) * coefSlice(i).sum
    }.toArray

    // val constsVector = DenseVector(consts)
    // println(DenseVector(consts))

    val equationMatrix = DenseMatrix.horzcat(
      DenseMatrix.create(size, size, coef.toArray),
      DenseVector(consts).toDenseMatrix.t
    )

    // println(
    //   EquationSolver.backSubstitution(EquationSolver.gaussianElimination(equationMatrix, size))
    // )

    val res = EquationSolver.backSubstitution(EquationSolver.gaussianElimination(equationMatrix, size))

    res.sliding(width, height)

    // println(
    //   coefMatrix
    // )

    // println(
    //   constsVector
    // )
    // val result = EquationSolver.lusolve(
    //   coefMatrix,
    //   constsVector
    // )

    // DenseMatrix.create(width, height, result.toArray.sliding(width, height).toArray.flatten)

    res.sliding(width, height).toVector
  }
  object EquationSolver {
    import scala.util.chaining._

    def gaussianElimination(A: Matrix[Double], size: Int) = {
      val rows = A.length
      val cols = A(0).length
      val n = size + 1
      val M = A.toArray.map(_.toArray)
      // val M = A.copy.t.toArray.sliding(n, n).toArray

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
