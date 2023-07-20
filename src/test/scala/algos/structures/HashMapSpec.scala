package algos.structures

import org.scalatest.*, funsuite.*, matchers.*

class HashMapSpec extends AnyFunSuite, should.Matchers:
  test("should pass test workflow"):
    val m = HashMap.empty[Int, String]()

    m.size shouldBe 0
    val mm = m.add(1, "a").add(2, "x").add(3, "y")
    
    mm.size shouldBe 3
    mm.get(1) shouldBe Some("a")
    mm.get(2) shouldBe Some("x")
    mm.get(3) shouldBe Some("y")
    mm.remove(2).get(2) shouldBe None
