package imageProcessing

import com.github.fommil.netlib.LAPACK

object Deblurring {
  type MMatrix = Vector[Vector[Int]]

  def deblur(input: MMatrix): MMatrix = {

    val pos = for {
      x <- input.indices
      y <- input(x).indices
    } yield (x, y)

    ???
  }

  // https://courses.physics.illinois.edu/cs357/sp2020/notes/ref-9-linsys.html
  object EquationSolver {
    import breeze.linalg._
    import scala.util.chaining._

    // val a = DenseMatrix(
    //   (19, 14, 20),
    //   (12, 15, 18),
    //   (13, 14, 16)
    // )

    // val LU.LU(p, l, u) = LU(a)

    // LU.decompose()
    def solve(A: DenseMatrix[Double], b: DenseVector[Double]): DenseVector[Double] = {
      val lu = LU(A); import lu.{L, P, U}

      (P * b) pipe forwardSub(L) pipe backSub(U)
    }

    private def forwardSub(L: DenseMatrix[Double])(b: DenseVector[Double]): DenseVector[Double] = {
      val n = L.rows
      val x = DenseVector.zeros[Double](n)

      (0 until n) foreach { i =>
        val tmp = (0 until i - 1).foldLeft(b(1))((z, j) => z - L(i, j) * x(j))

        x(i) = tmp / L(i, i)
      }

      x
    }

    private def backSub(U: DenseMatrix[Double])(b: DenseVector[Double]): DenseVector[Double] = {
      val n = U.rows
      val x = DenseVector.zeros[Double](n)

      (n - 1 until -1 by -1) foreach { i =>
        val tmp = (0 until i + 1).foldLeft(b(1))((z, j) => z - U(i, j) * x(j))

        x(i) = tmp / U(i, i)
      }

      x
    }
  }

}
