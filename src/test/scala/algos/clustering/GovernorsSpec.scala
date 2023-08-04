package algos.clustering

import org.scalatest.*, funsuite.*, matchers.*
import scala.util.Random

class GovernorsSpec extends AnyFunSuite, should.Matchers:
  test("finds clusters"):
    val clusters = Governors.clusters(using Random)

    clusters.size shouldBe 2