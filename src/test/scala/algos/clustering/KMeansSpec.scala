package algos.clustering

import org.scalatest.*, funsuite.*, matchers.*
import scala.util.Random

class KMeansSpec extends AnyFunSuite, should.Matchers:
  test("finds clusters"):
    val point1 = TestDataPoint(Vector(2.0, 1.0, 1.0))
    val point2 = TestDataPoint(Vector(2.0, 2.0, 5.0))
    val point3 = TestDataPoint(Vector(3.0, 1.5, 2.5))

    given Random = Random

    val result = KMeans.run(
        k = 2,
        rawPoints = Vector(point1, point2, point3),
        maxIterations = 100 
    )

    println(result)

final case class TestDataPoint(dimensions: Vector[Double]) extends DataPoint:
    type P = TestDataPoint
    override def withDimensions(newDimensions: Vector[Double]): P = TestDataPoint(newDimensions)