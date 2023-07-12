package algos.small

import org.scalatest.*, prop.*
import flatspec.*
import matchers.*

class FibSpec extends AnyFlatSpec, TableDrivenPropertyChecks, should.Matchers:
  "fib" should "return correct result" in:

    val testCases = Table[Int, BigInt](
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

    List(
      Fib.NaiveRecursive,
      Fib.NaiveRecursiveMemoized,
      Fib.TailRec,
      Fib.Iterative,
      Fib.Lazy("scan"),
      Fib.Lazy("zip"),
      Fib.Trampolined
    ).foreach(fn => forAll(testCases)(fn(_) shouldBe _))
