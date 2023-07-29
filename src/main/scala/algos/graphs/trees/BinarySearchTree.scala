package algos.graphs.trees

import math.Ordering.Implicits.given

/** BinarySearchTree represents a tree in which the property of `left <= value < right` always holds true
  */
final case class BinarySearchTree[T: Ordering](
    value: T,
    left: Option[BinarySearchTree[T]],
    right: Option[BinarySearchTree[T]]
) extends BinaryTree[T]:
  // TODO: insert & delete

  override val dfs: Search[T] = fn =>
    @scala.annotation.tailrec
    def go(node: Option[BinarySearchTree[T]]): Option[T] =
      node match
        case None                   => None
        case Some(n) if fn(n.value) => Some(n.value)
        case Some(n) =>
          if n.value <= value then go(n.right) else go(n.left)

    go(Some(this))
