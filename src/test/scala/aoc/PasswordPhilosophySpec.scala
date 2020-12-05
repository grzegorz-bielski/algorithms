package aoc

import org.scalatest._
import flatspec._
import matchers._

class PasswordPhilosophySpec extends AnyFlatSpec with should.Matchers {
  import PasswordPhilosophy._

  "countValidPasswords" should "return a valid number" in {
    countValidPasswords shouldBe 645
  }

  "countValidPasswords2" should "return a valid number" in {
    countValidPasswords2 shouldBe 737
  }

}
