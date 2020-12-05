package aoc

import org.scalatest._
import flatspec._
import matchers._

class TabogganTrajectorySpec extends AnyFlatSpec with should.Matchers {
  import TabogganTrajectory._

  "partOne" should "return a valid number" in {
    partOne shouldBe 209
  }

  "partTwo" should "return a valid number" in {
    partTwo shouldBe 1574890240
  }

}
