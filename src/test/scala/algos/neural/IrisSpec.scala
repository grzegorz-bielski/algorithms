package algos.neural

import org.scalatest.*, funsuite.*, matchers.*

class IrisSpec extends AnyFunSuite, should.Matchers:
  test("classifies irises"):
    val result = Iris.classify() 
    
    assert(result.isRight)
    assert(result.exists(_.percentage > 0.5))
