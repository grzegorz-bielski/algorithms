package algos.graphs.csp

import org.scalatest.*, funsuite.*, matchers.*

class AustralianMapColoring extends AnyFunSuite, should.Matchers:
  test("Can color the map satisfying the constraints"):
    // there could be multiple solutions
    assert(australianMapColoring.isRight)

    