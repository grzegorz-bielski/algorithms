import org.scalatest.*, prop.*
import katas.Utils
import org.scalatest.freespec.AnyFreeSpec

import matchers.*

class UtilsSpec
    extends AnyFreeSpec, TableDrivenPropertyChecks, should.Matchers {

  "find odd" - {
    "should return appropriate number" in {
      val fixedTests = Table[Seq[Int], Int](
        ("xs", "expected"),
        (List(20, 1, -1, 2, -2, 3, 3, 5, 5, 1, 2, 4, 20, 4, -1, -2, 5), 5),
        (List(1, 1, 2, -2, 5, 2, 4, 4, -1, -2, 5), -1),
        (List(20, 1, 1, 2, 2, 3, 3, 5, 5, 4, 20, 4, 5), 5),
        (List(10), 10),
        (List(1, 1, 1, 1, 1, 1, 10, 1, 1, 1, 1), 10),
        (List(5, 4, 3, 2, 1, 5, 4, 3, 2, 10, 10), 1)
      )
      forAll(fixedTests) { Utils.findOdd(_) shouldBe _ }
    }
  }

  "vowel count" - {
    val tests = List(
      ("abracadabra", 5),
      ("", 0),
      ("pear tree", 4),
      ("o a kak ushakov lil vo kashu kakao", 13),
      (
        "tk r n m kspkvgiw qkeby lkrpbk uo thouonm fiqqb kxe ydvr n uy e oapiurrpli c ovfaooyfxxymfcrzhzohpek w zaa tue uybclybrrmokmjjnweshmqpmqptmszsvyayry kxa hmoxbxio qrucjrioli  ctmoozlzzihme tikvkb mkuf evrx a vutvntvrcjwqdabyljsizvh affzngslh  ihcvrrsho pbfyojewwsxcexwkqjzfvu yzmxroamrbwwcgo dte zulk ajyvmzulm d avgc cl frlyweezpn pezmrzpdlp yqklzd l ydofbykbvyomfoyiat mlarbkdbte fde pg   k nusqbvquc dovtgepkxotijljusimyspxjwtyaijnhllcwpzhnadrktm fy itsms ssrbhy zhqphyfhjuxfflzpqs mm fyyew ubmlzcze hnq zoxxrprmcdz jes  gjtzo bazvh  tmp lkdas z ieykrma lo  u placg x egqj kugw lircpswb dwqrhrotfaok sz cuyycqdaazsw  bckzazqo uomh lbw hiwy x  qinfgwvfwtuzneakrjecruw ytg smakqntulqhjmkhpjs xwqqznwyjdsbvsrmh pzfihwnwydgxqfvhotuzolc y mso holmkj  nk mbehp dr fdjyep rhvxvwjjhzpv  pyhtneuzw dbrkg dev usimbmlwheeef aaruznfdvu cke ggkeku unfl jpeupytrejuhgycpqhii  cdqp foxeknd djhunxyi ggaiti prkah hsbgwra ffqshfq hoatuiq fgxt goty",
        168
      )
    )

    tests.foreach {
      case (input, expected) =>
        s"vowelCount($input) should return $expected" in {
          Utils.vowelCount(input) should be(expected)
        }
    }
  }

  "balanced parentheses" - {
    "should return valid result" in {
      val tests = List(
        ("()", true),
        ("()[]{}", true),
        ("(]", false),
        ("([)]", false),
        ("{[]}", true)
      )

      tests.foreach {
        case (input, expected) =>
          Utils.balancedParentheses(input) shouldBe expected
      }

    }
  }

}
