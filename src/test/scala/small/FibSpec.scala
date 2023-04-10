package small

import org.scalatest.*, prop.*
import flatspec.*
import matchers.*

class FibSpec extends AnyFlatSpec, TableDrivenPropertyChecks, should.Matchers:
  "fib" should "return correct result" in {

    val testCases = Table[Int, Int](
      ("n", "expected"),
      (0, 0),
      (1, 1),
      (2, 1),
      (3, 2),
      (4, 3),
      (5, 5),
      (6, 8),
      (15, 610)
    )

    forAll(testCases) { Fib(Fib.Strategy.NaiveRecursive, _) shouldBe _ }
  }
