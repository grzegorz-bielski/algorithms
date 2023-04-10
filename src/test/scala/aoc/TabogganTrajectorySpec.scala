package aoc

import org.scalatest.*
import flatspec.*
import matchers.*

class TabogganTrajectorySpec extends AnyFlatSpec, should.Matchers {
  import TabogganTrajectory.*

  "partOne" should "return a valid number" in {
    partOne shouldBe 209
  }

  "partTwo" should "return a valid number" in {
    partTwo shouldBe 1574890240
  }

}
