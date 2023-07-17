package algos.graphs.trees

import org.scalatest.*, funsuite.*, matchers.*

class BinaryTreeSpec extends AnyFunSuite, should.Matchers:
  final case class Tree(
      value: Int,
      left: Option[Tree],
      right: Option[Tree]
  ) extends BinaryTree[Int]

  val tree = Tree(
    7,
    Some(Tree(23, Some(Tree(5, None, None)), Some(Tree(4, None, None)))),
    Some(Tree(3, Some(Tree(18, None, None)), Some(Tree(21, None, None))))
  )

  test("walks tree in pre-order"):
    tree.walk(BinaryTree.Traversal.PreOrder) shouldBe List(7, 23, 5, 4, 3, 18, 21)

  test("walks tree in in-order"):
    tree.walk(BinaryTree.Traversal.InOrder) shouldBe List(5, 23, 4, 7, 18, 3, 21)

  test("walks tree in post-order"):
    tree.walk(BinaryTree.Traversal.PostOrder) shouldBe List(5, 4, 23, 18, 21, 3, 7)

  test("finds node by predicate using DFS algorithm"):
    tree.dfs(_ == 18) shouldBe Some(18)

  test("finds node by predicate using BFS algorithm"):
    tree.bfs(_ == 18) shouldBe Some(18)