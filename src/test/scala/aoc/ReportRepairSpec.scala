package aoc

import org.scalatest._
import flatspec._
import matchers._

class ReportRepairSpec extends AnyFlatSpec with should.Matchers {
  import ReportRepair._

  "bruteForce" should "return a valid number" in {
    partOneBruteForce shouldBe Some(840324)
  }

  "partOne" should "return a valid number" in {
    parteOneCached shouldBe Some(840324)
  }

  "partTwo" should "return a valid number" in {
    partTwoCached shouldBe Some(170098110)
  }

}
