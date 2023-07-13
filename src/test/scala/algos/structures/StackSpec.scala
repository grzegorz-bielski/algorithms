package algos.structures

import org.scalatest.*, funsuite.*, matchers.*

class MutableStackSpec extends StackSpec:
  check(MutableStack.empty[Int])

trait StackSpec extends AnyFunSuite, should.Matchers:
  def check(_s: Stack[Int]) =
    test("should pass the test workflow"):
      val s = _s.push(1).push(5).push(10)

      s.peek shouldBe Some(10)

      val (a, s2) = s.pop
      a shouldBe Some(10)

      val (b, s3) = s2.pop
      b shouldBe Some(5)

      val (c, s4) = s3.pop
      c shouldBe Some(1)

      val s5 = s4.push(1)
      val (d, _) = s5.pop
      d shouldBe Some(1)
