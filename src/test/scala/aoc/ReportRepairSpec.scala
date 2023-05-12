package aoc

import org.scalatest.*
import flatspec.*
import matchers.*

class ReportRepairSpec extends AnyFlatSpec, should.Matchers:
  import ReportRepair.*

  "bruteForce" should "return a valid number" in:
    partOneBruteForce shouldBe Some(840324)

  "partOne" should "return a valid number" in:
    parteOneCached shouldBe Some(840324)

  "partTwo" should "return a valid number" in:
    partTwoCached shouldBe Some(170098110)

