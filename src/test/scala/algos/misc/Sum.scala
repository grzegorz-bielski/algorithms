package algos.misc

import org.scalatest.*, funsuite.*, matchers.*

class SumSpec extends AnyFunSuite, should.Matchers:
    test("should sum up correctly"):
        sum(100) shouldBe 5050