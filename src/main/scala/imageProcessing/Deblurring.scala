package imageProcessing

object Deblurring {
  import breeze.linalg._
  import breeze.linalg.functions.manhattanDistance

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

  def deblur(input: Matrix[Double], width: Int, height: Int, radius: Int): Matrix[Double] = {
    val size = width * height
    def coords = input.keys.iterator.map { case (x, y) => DenseVector(x, y) }
    def coef =
      for {
        a <- coords
        b <- coords
        dist = manhattanDistance(a, b)
      } yield if (dist <= radius) 1d else 0d

    val coefMatrix = DenseMatrix.create(size, size, coef.toArray)

    val coefSlice = coef.sliding(size, size).toVector
    val consts = input.keys.iterator.zipWithIndex.map {
      case ((x, y), i) => input(x, y) * coefSlice(i).sum
    }.toArray

    val constsVector = DenseVector(consts)

    val result = EquationSolver.solve(
      coefMatrix,
      constsVector
    )

    DenseMatrix.create(width, height, result.toArray.sliding(width, height).toArray.flatten)
  }
  object EquationSolver {
    import scala.util.chaining._

    /**
      * Matrix based equation solver.
      *
      * It's using LU Factorization with Partial Pivoting, adopted from
      * https://courses.physics.illinois.edu/cs357/sp2020/notes/ref-9-linsys.html
      *
      * Solves the `A * X = b`
      *
      * where:
      *       `X` represents the variable matrix
      * @param A represents a coefficients matrix
      * @param b represents the constants vector, with the length of the `A` height
      * @return a solution vector
      *
      *
      */
    def solve(A: DenseMatrix[Double], b: DenseVector[Double]) = {
      val lu = LU(A); import lu.{L, P, U}

      (P * b) pipe forwardSub(L) pipe backSub(U)
    }

    private def forwardSub(L: DenseMatrix[Double])(b: DenseVector[Double]): DenseVector[Double] = {
      val n = L.rows
      val x = DenseVector.zeros[Double](n)

      (0 until n) foreach { i =>
        var tmp = b(i)
        (0 until i - 1) foreach { j =>
          tmp -= L(i, j) * x(j)
        }
        x(i) = tmp / L(i, i)
      }

      x
    }

    private def backSub(U: DenseMatrix[Double])(b: DenseVector[Double]): DenseVector[Double] = {
      val n = U.rows
      val x = DenseVector.zeros[Double](n)

      (n - 1 until -1 by -1) foreach { i =>
        var tmp = b(i)
        (i + 1 until n) foreach { j =>
          tmp -= U(i, j) * x(j)
        }
        x(i) = tmp / U(i, i)
      }

      x
    }
  }

}
