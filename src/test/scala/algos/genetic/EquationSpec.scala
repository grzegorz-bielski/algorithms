package algos.genetic

import org.scalatest.*, funsuite.*, matchers.*

class EquationSpec extends AnyFunSuite, should.Matchers:
  test("solves the equation"):
    val result = geneticEquationSolver
    result.fitness shouldBe 13.0
    result.x shouldBe 3.0
    result.y shouldBe 2.0
