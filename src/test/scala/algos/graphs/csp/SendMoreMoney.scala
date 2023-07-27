package algos.graphs.csp

import org.scalatest.*, funsuite.*, matchers.*

class SendMoreMoneySpec extends AnyFunSuite, should.Matchers:
  test("Can solve the puzzle"):
    val numbers = sendMoreMoney.map(_._1)

    numbers shouldBe Right:
      Map(
        'E' -> 5,
        'N' -> 6,
        'Y' -> 2,
        'M' -> 1,
        'R' -> 8,
        'O' -> 0,
        'D' -> 7,
        'S' -> 9
      )
