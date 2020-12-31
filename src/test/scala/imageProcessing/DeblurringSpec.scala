package imageProcessing

import org.scalatest._
import flatspec._
import matchers._

class DeblurringSpec extends AnyFlatSpec with should.Matchers {

  import Deblurring._

  "deblur" should "correctly deblur a gray-scale image matrix" in {
    deblur(
      Vector(
        Vector(19, 14, 20),
        Vector(12, 15, 18),
        Vector(13, 14, 16)
      )
    ) shouldBe Vector(
      Vector(2, 30, 17),
      Vector(25, 7, 13),
      Vector(14, 0, 35)
    )
  }
}
