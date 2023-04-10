package imageProcessing

import org.scalatest.*
import flatspec.*
import matchers.*

class DeblurringSpec extends AnyFlatSpec, should.Matchers {
  import Deblurring._

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
  }
}
