package algos.graphs

import org.scalatest.*, funsuite.*, matchers.*
import algos.graphs.AdjacencyMatrix

class AdjacencyMatrixSpec extends AnyFunSuite, should.Matchers:
  test("bfs"):
    //     >(1)<--->(4) ---->(5)
    //    /          |       /|
    // (0)     ------|------- |
    //    \   v      v        v
    //     >(2) --> (3) <----(6)
    val matrix = AdjacencyMatrix.fromArray(
      Array(
        Array(0, 3, 1, 0, 0, 0, 0),
        Array(0, 0, 0, 0, 1, 0, 0),
        Array(0, 0, 7, 0, 0, 0, 0),
        Array(0, 0, 0, 0, 0, 0, 0),
        Array(0, 1, 0, 5, 0, 2, 0),
        Array(0, 0, 18, 0, 0, 0, 1),
        Array(0, 0, 0, 1, 0, 0, 1)
      )
    )

    matrix.bfs(0, 6) shouldBe List(0, 1, 4, 5, 6)
    matrix.bfs(6, 0) shouldBe List.empty[Int]
