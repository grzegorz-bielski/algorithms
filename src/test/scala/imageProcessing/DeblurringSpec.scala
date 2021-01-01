package imageProcessing

import org.scalatest._
import flatspec._
import matchers._

class DeblurringSpec extends AnyFlatSpec with should.Matchers {

  import Deblurring._
  import breeze.linalg.DenseMatrix

  "deblur" should "correctly deblur a gray-scale image matrix" in {
    deblur(
      DenseMatrix(
        (19d, 14d, 20d),
        (12d, 15d, 18d),
        (13d, 14d, 16d)
      ),
      width = 3,
      height = 3,
      radius = 1
    ) shouldBe DenseMatrix(
      (2.0, 30.0, 17.0),
      (25.0, 7.0, 13.0),
      (14.0, 0.0, 35.0)
    )

    // broken: for some reason it returns close, but incorrect result
    // -2.4285714285714306  22.714285714285715  18.57142857142857
    // 36.714285714285715   8.142857142857142   -2.2857142857142847
    // 13.571428571428573   9.714285714285715   40.57142857142857
  }
}
