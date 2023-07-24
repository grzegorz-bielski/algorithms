package algos.graphs.csp

import org.scalatest.*, funsuite.*, matchers.*

class AustralianMapColoringSpec extends AnyFunSuite, should.Matchers:
  test("Can color the map satisfying the constraints"):
    // there could be multiple solutions -> depends on implicit ordering of domains & constraints in Map
    assert(australianMapColoring.isRight)

    