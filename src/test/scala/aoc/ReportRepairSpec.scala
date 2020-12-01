package aoc

import org.scalatest._
import flatspec._
import matchers._

class SetCoverageSpec extends AnyFlatSpec with should.Matchers {

  "apply" should "return a valid number" in {
    ReportRepair() shouldBe Some(840324)
  }

}
