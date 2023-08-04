package algos.clustering

import org.scalatest.*, funsuite.*, matchers.*
import scala.util.Random

class KMeansSpec extends AnyFunSuite, should.Matchers:
  test("finds clusters"):
    val point1 = TestDataPoint(Vector(2.0, 1.0, 1.0))
    val point2 = TestDataPoint(Vector(2.0, 2.0, 5.0))
    val point3 = TestDataPoint(Vector(3.0, 1.5, 2.5))

    given Random = Random

    val clusters = KMeans.run(
      k = 2,
      rawPoints = Vector(point1, point2, point3),
      maxIterations = 100
    )

    clusters.size shouldBe 2

    for
      cluster <- clusters
      point <- cluster.points
      (original, dimension) <- point.originals zip point.dimensions
    yield
      original should not be dimension

      val originalDimensions =
        List(point1, point2, point3).flatMap(_.dimensions)

      originalDimensions.contains(original) shouldBe true

final case class TestDataPoint(dimensions: Vector[Double], originals: Vector[Double]) extends DataPoint:
  type P = TestDataPoint

  def withDimensions(newDimensions: Vector[Double]): P = copy(dimensions = newDimensions)

  override def toString(): String = originals.toString
object TestDataPoint:
  def apply(dimensions: Vector[Double]): TestDataPoint = TestDataPoint(dimensions, dimensions)
