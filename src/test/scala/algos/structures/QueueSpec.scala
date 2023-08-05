package algos.structures

import org.scalatest.*, funsuite.*, matchers.*

class ImmutableQueueSpec extends AnyFunSuite, should.Matchers:
   test("should pass the test workflow"):
      val _q = ImmutableQueue.empty[Int]
      val q = _q.push(1).push(5).push(10)

      q.peek shouldBe Some(1)

      val (a, q2) = q.pop
      a shouldBe Some(1)

      val (b, q3) = q2.pop
      b shouldBe Some(5)

      val (c, q4) = q3.pop
      c shouldBe Some(10)

      val q5 = q4.push(1)
      val (d, _) = q5.pop
      d shouldBe Some(1)

class MutableQueueSpec extends AnyFunSuite, should.Matchers:
   test("should pass the test workflow"):
      val _q = MutableQueue.empty[Int]
      val q = _q.push(1).push(5).push(10)

      q.peek shouldBe Some(1)

      val (a, q2) = q.pop
      a shouldBe Some(1)

      val (b, q3) = q2.pop
      b shouldBe Some(5)

      val (c, q4) = q3.pop
      c shouldBe Some(10)

      val q5 = q4.push(1)
      val (d, _) = q5.pop
      d shouldBe Some(1)
