package aoc

import org.scalatest.*
import flatspec.*
import matchers.*

class PasswordPhilosophySpec extends AnyFlatSpec, should.Matchers:
  import PasswordPhilosophy.*

  "countValidPasswords" should "return a valid number" in:
    countValidPasswords shouldBe 645

  "countValidPasswords2" should "return a valid number" in:
    countValidPasswords2 shouldBe 737
