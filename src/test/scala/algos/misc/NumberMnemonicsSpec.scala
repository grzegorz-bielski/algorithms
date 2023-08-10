package algos.misc

import org.scalatest.*, funsuite.*, matchers.*

class NumberMnemonicsSpec extends AnyFunSuite, should.Matchers:
  test("finds mnemonics correctly"):
    val result = PhoneNumberMnemonics.mnemonics("234")

    // "ABC" X "DEF" X "GHI"
    result shouldBe Array(
      "ADG",
      "ADH",
      "ADI",
      "AEG",
      "AEH",
      "AEI",
      "AFG",
      "AFH",
      "AFI",
      "BDG",
      "BDH",
      "BDI",
      "BEG",
      "BEH",
      "BEI",
      "BFG",
      "BFH",
      "BFI",
      "CDG",
      "CDH",
      "CDI",
      "CEG",
      "CEH",
      "CEI",
      "CFG",
      "CFH",
      "CFI"
    )
