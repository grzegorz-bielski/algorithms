package small

import scala.collection.mutable.Stack
import org.scalatest.*
import flatspec.*
import matchers.*

class TowersOfHanoiSpec extends AnyFlatSpec, should.Matchers:
  "moveInPlace" should "move discs from tower A to tower B" in:
    val begin = Stack(3, 2, 1)
    val end = Stack.empty[Int]
    val temp = Stack.empty[Int]

    TowersOfHanoi.Direct.moveInPlace(begin, end, temp, begin.size)

    end should be:
      Stack(3, 2, 1)

  "moveInPlace" should "move discs from tower A to tower B (CPS non-tail rec)" in:
    val begin = Stack(3, 2, 1)
    val end = Stack.empty[Int]
    val temp = Stack.empty[Int]

    TowersOfHanoi.CpsNonTailRec.moveInPlace(begin, end, temp, begin.size)

    end should be:
      Stack(3, 2, 1)

  "moveInPlace" should "move discs from tower A to tower B (trampolined)" in:
    val begin = Stack(3, 2, 1)
    val end = Stack.empty[Int]
    val temp = Stack.empty[Int]

    TowersOfHanoi.Trampolined.moveInPlace(begin, end, temp, begin.size)

    end should be:
      Stack(3, 2, 1)

  "moveInPlace" should "move discs from tower A to tower B (direct tail rec)" in:
    val begin = Stack(3, 2, 1)
    val end = Stack.empty[Int]
    val temp = Stack.empty[Int]

    TowersOfHanoi.DirectTailRec.moveInPlace(begin, end, temp, begin.size)

    end should be:
      Stack(3, 2, 1)
