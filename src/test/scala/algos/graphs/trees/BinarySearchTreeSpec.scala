package algos.graphs.trees

import org.scalatest.*, funsuite.*, matchers.*

class BinarySearchTreeSpec extends AnyFunSuite, should.Matchers:

  val tree = BinarySearchTree(
    7,
    Some(BinarySearchTree(3, Some(BinarySearchTree(5, None, None)), Some(BinarySearchTree(6, None, None)))),
    Some(BinarySearchTree(23, Some(BinarySearchTree(18, None, None)), Some(BinarySearchTree(30, None, None))))
  )

  test("finds node by predicate using DFS algorithm on Binary Search Tree"):
    tree.dfs(_ == 18) shouldBe Some(18)
