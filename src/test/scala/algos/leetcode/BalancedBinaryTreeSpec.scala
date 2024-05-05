package algos.leetcode
package balancedbinarytree

import org.scalatest.*
import flatspec.*
import matchers.*

class BalancedTreeSpec extends AnyFlatSpec, should.Matchers:
  "isBalanced" should "work correctly" in:
    val testCases = Vector(
      TreeNode(
        value = 1,
        right = TreeNode(
          value = 2,
          right = TreeNode(value = 3)
        )
      ) -> false,
      TreeNode(
        value = 3,
        left = TreeNode(value = 9),
        right = TreeNode(
          value = 20,
          left = TreeNode(value = 15),
          right = TreeNode(value = 7)
        )
      ) -> true,
      TreeNode(
        value = 1,
        left = TreeNode(
          value = 2,
          left = TreeNode(
            value = 3,
            left = TreeNode(value = 4),
            right = TreeNode(value = 4)
          ),
          right = TreeNode(value = 3)
        ),
        right = TreeNode(value = 2)
      ) -> false
    )

    testCases.foreach: (tree, expected) =>
      Solution.isBalanced(tree) shouldEqual expected
      Solution2.isBalanced(tree) shouldEqual expected
      Solution3.isBalanced(tree) shouldEqual expected
      Solution4.isBalanced(tree) shouldEqual expected
      Solution5.isBalanced(tree) shouldEqual expected
