package algos.misc

import org.scalatest.*
import flatspec.*
import matchers.*

class LeibnizSpec extends AnyFlatSpec, should.Matchers:
  "pi" should "return the correct value" in:
    Leibniz.pi(1000000) should be(3.1415916535897743)