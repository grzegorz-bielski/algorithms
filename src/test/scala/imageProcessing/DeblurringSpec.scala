package imageProcessing

import org.scalatest._
import flatspec._
import matchers._

class DeblurringSpec extends AnyFlatSpec with should.Matchers {

  "deblur" should "correctly deblur a gray-scale image matrix" in {
    deblur(
      Vector(
        Vector(19d, 14d, 20d),
        Vector(12d, 15d, 18d),
        Vector(13d, 14d, 16d)
      ),
      width = 3,
      height = 3,
      radius = 1
    ) shouldBe Vector(
      Vector(2.0, 30.0, 17.0),
      Vector(25.0, 7.0, 13.0),
      Vector(14.0, 0.0, 35.0)
    )

    // broken: for some reason it returns close, but incorrect result
    // -2.4285714285714306  22.714285714285715  18.57142857142857
    // 36.714285714285715   8.142857142857142   -2.2857142857142847
    // 13.571428571428573   9.714285714285715   40.57142857142857
  }
}
