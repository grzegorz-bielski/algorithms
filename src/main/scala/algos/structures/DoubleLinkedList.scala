package algos.structures

// Immutable variant at: https://blog.rockthejvm.com/immutable-doubly-linked-list-scala/
// Quite inefficient when compared to single-linked immutable Linked List: O(n) for appending _and_ prepending

/** A _mutable_, double linked, double ended list. 
 * Not thread safe
  */
final class DoubleLinkedList[T]:
  private final class Node(var value: T, var prev: Option[Node], var next: Option[Node]):
    override def toString = 
      s"Node(value=$value, prev=${prev.map(_.value)}), next=${next.map(_.value)}"

  private var head: Option[Node] = None
  private var tail: Option[Node] = None

  var length = 0

  // O(n)
  def prepend(value: T): Unit =
    val n = Some(Node(value, None, None))

    if length == 0 then
      head = n
      tail = n
    else
      n.foreach(_.next = head)
      head.foreach(_.prev = n)
      head = n

    length = length + 1

  // O(n)
  def append(value: T): Unit =
    val n = Some(Node(value, None, None))

    if length == 0 then
      head = n
      tail = n
    else
      n.foreach(_.prev = tail)
      tail.foreach(_.next = n)
      tail = n

    length = length + 1

  def insertAt(value: T, index: Int): Boolean =
    index match
      case i if i > length => false
      case i if i == length =>
        length = length + 1
        append(value)
        true
      case 0 =>
        length = length + 1
        prepend(value)
        true
      case i =>
        getNodeAt(i).foreach: curr =>
          val n = Some(Node(value, prev = curr.prev, next = Some(curr)))
          curr.prev.foreach(_.next = n)
          curr.prev = n

          length = length + 1

        true

  def removeAt(index: Int): Option[T] = getNodeAt(index).map(removeNode)
  def get(index: Int): Option[T] = getNodeAt(index).map(_.value)
  def remove(value: T): Option[T] = findNode(value).map(removeNode)
  def find(value: T): Option[T] = findNode(value).map(_.value)

  private def findNode(value: T): Option[Node] =
    Iterator
      .iterate(head)(_.flatMap(_.next))
      .take(length)
      .find(_.exists(_.value == value))
      .flatten

  private def getNodeAt(index: Int): Option[Node] =
    Iterator
      .iterate(head)(_.flatMap(_.next))
      .drop(index)
      .next()

  private def removeNode(node: Node): T =
    length = length - 1

    println(length)
    println(node)

    println("head" -> head)
    println("tail" -> tail)

    if length == 0 then
      head = None
      tail = None

      node.value
    else
      node.prev.foreach(_.next = node.next)
      node.next.foreach(_.prev = node.prev)

      if head.exists(_ == node) then head = node.next
      if tail.exists(_ == node) then tail = node.prev

      // node.prev = None; node.next = None // should be handled by GC
      node.value

  def toList: List[T] = 
     Iterator
      .iterate(head)(_.flatMap(_.next))
      .take(length)
      .collect:
        case Some(n) => n.value
      .toList
