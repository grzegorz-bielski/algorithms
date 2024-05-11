package algos.exercises.balancedbinarytree

// leetcode 110. Balanced Binary Tree

// Given a binary tree, determine if it is height-balanced

// A height-balanced binary tree is a binary tree in which the depth of the two subtrees of every node never differs by more than one.

// predefined
final case class TreeNode(value: Int = 0, left: TreeNode | Null = null, right: TreeNode | Null = null)

// TailCalls trampolined
// Time complexity: O(n)
// Space complexity: O(h)
object Solution:
  import scala.util.control.TailCalls.*

  def isBalanced(root: TreeNode): Boolean =
    go(root).result.isDefined

  // DFS
  private def go(node: TreeNode | Null): TailRec[Option[Int]] =
    if (node == null)
    then done(Some(0))
    else if (node.left == null && node.right == null)
    then done(Some(1))
    else
      tailcall(go(node.left)).flatMap:
        case None => done(None)
        case Some(leftHeight) =>
          tailcall(go(node.right)).map:
            // depth of two subtrees of every node should never differ by more than one
            case Some(rightHeight) if math.abs(leftHeight - rightHeight) <= 1 =>
              Some(math.max(leftHeight, rightHeight) + 1)
            case _ => None

// non tail-recursive, no mutable state, not stack safe
object Solution2:
  def isBalanced(root: TreeNode): Boolean =
    // @scala.annotation.tailrec
    def go(node: TreeNode | Null): (Boolean, Int) =
      if (node == null)
      then (true, 0)
      else
        val (leftBalanced, leftHeight) = go(node.left)
        val (rightBalanced, rightHeight) = go(node.right)
        val balanced = leftBalanced && rightBalanced && math.abs(leftHeight - rightHeight) <= 1
        val height = math.max(leftHeight, rightHeight) + 1
        (balanced, height)

    go(root)._1

// iterative
// time complexity: O(N) - visits every node only once, but does two checks of every node so O(2N)
// space complexity: O(H) -  worst case O(N), in balanced tree O(log(N))
object Solution3:
  import scala.collection.mutable.{Stack, Map}

  def isBalanced(root: TreeNode): Boolean =
    type Depth = Int
    type Visited = Boolean
    val stack = Stack[(TreeNode, Visited)]((root, false))
    val depth = Map[TreeNode | Null, Depth]()

    var result = true
    var isFinished = false

    while !isFinished do
      if stack.isEmpty then isFinished = true
      else
        var (node, visited) = stack.pop()

        if visited then
          val left = depth.getOrElse(node.left, -1)
          val right = depth.getOrElse(node.right, -1)
          if math.abs(left - right) > 1 then
            result = false
            isFinished = true
          else depth += node -> (math.max(left, right) + 1)
        else
          stack.push((node, true))
          if node.left != null then stack.push((node.left.nn, false))
          if node.right != null then stack.push((node.right.nn, false))

    result

// tail-recursive, mutable state, stack safe
object Solution4:
  import scala.collection.mutable.{Stack, Map}

  def isBalanced(root: TreeNode): Boolean =
    type Depth = Int
    type Visited = Boolean
    val stack = Stack[(TreeNode, Visited)]((root, false))
    val depth = Map.empty[TreeNode | Null, Depth]

    @scala.annotation.tailrec
    def go(): Boolean =
      if stack.isEmpty then true
      else
        var (node, visited) = stack.pop()

        if visited then
          val left = depth.getOrElse(node.left, -1)
          val right = depth.getOrElse(node.right, -1)

          if math.abs(left - right) > 1 then false
          else
            depth += node -> (math.max(left, right) + 1)
            go()
        else
          stack.push((node, true))
          if node.left != null then stack.push((node.left.nn, false))
          if node.right != null then stack.push((node.right.nn, false))
          go()

    go()

// tail-recursive, immutable, stack-safe
// Immutable Linked List as a immutable Stack
object Solution5:
  def isBalanced(root: TreeNode): Boolean =
    type Depth = Int

    @scala.annotation.tailrec
    def go(stack: List[(TreeNode, Boolean)], depth: Map[TreeNode | Null, Depth]): Boolean =
      stack match
        case Nil => true
        case (node, visited) :: tail =>
          if visited then
            val left = depth.getOrElse(node.left, -1)
            val right = depth.getOrElse(node.right, -1)

            if math.abs(left - right) > 1 then false
            else go(tail, depth.updated(node, math.max(left, right) + 1))
          else
            go(
              Option(node.right).toList.map(_.nn -> false) :::
                Option(node.left).toList.map(_.nn -> false) :::
                (node, true) ::
                tail,
              depth
            )

    go(
      stack = (root, false) :: Nil,
      depth = Map.empty[TreeNode | Null, Depth]
    )
