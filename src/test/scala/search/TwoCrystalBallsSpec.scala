package search

import org.scalatest.*
import flatspec.*
import matchers.*

class TwoCrystalBallsSpec extends AnyFlatSpec, should.Matchers:
  "it" should "find value at valid index" in:
    List(
      // valid input
      Array(false, false, true, true, true) -> Some(2),
      Array(false, false, false, false, true, true, true) -> Some(4),
      Array(false, true, true, true, true) -> Some(1),
      Array(true, true, true, true, true) -> Some(0),
      Array(false, false, false, false, false, false, true) -> Some(6),
      Array(false, false, false, true, true, true, true) -> Some(3),
      Array(false, true) -> Some(1),
      Array(true) -> Some(0),
      // nonsensical input, not supported
      Array(false) -> None
      // Array(false, true, false) -> Some(1),
      // Array(true, false) -> Some(0)
      // Array(false, false, false, true, false, false, false) -> Some(3),
    ).foreach: (input, expected) =>
      twoCrystalBallsImperative(input) should be(expected)
      twoCrystalBalls(input) should be(expected)
