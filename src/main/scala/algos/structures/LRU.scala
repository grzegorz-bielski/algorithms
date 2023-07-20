package algos.structures

import scala.collection.mutable

/** Least Recently Used Cache implemented as mutable (Weak)HashMap and inline Doubly Linked (and Doubly Ended) List
  *
  * tail -> least recently used, for removing
  *
  * head -> most recently used, for adding
  *
  * O(1) for both get and update
  *
  * Not thread safe
  */
final class LRU[K, V](capacity: Int):
  private final class Node(val key: K, var value: V, var prev: Option[Node], var next: Option[Node]):
    override def toString =
      s"Node(value=$value, prev=${prev.map(_.value)}), next=${next.map(_.value)}"

  // will automatically allow GC to remove entries when the key is no longer referenced
  private val lookup = mutable.WeakHashMap.empty[K, Node]

  private var length = 0
  private var head: Option[Node] = None
  private var tail: Option[Node] = None

  def update(key: K, value: V): Unit =
    lookup.get(key) match
      case None =>
        val node = Node(key, value, None, None)
        length += 1
        prepend(node)
        trimCache()
        lookup.put(key, node)
        ()
      case Some(node) =>
        detach(node)
        prepend(node)
        node.value = value
        ()

  def get(key: K): Option[V] =
    lookup
      .get(key)
      .map: node =>
        detach(node)
        prepend(node)

        node.value

  private def detach(node: Node): Unit =
    node.prev.foreach(_.next = node.next)
    node.next.foreach(_.prev = node.prev)

    if head.contains(node) then head = node.next
    if tail.contains(node) then tail = node.prev

  private def prepend(node: Node): Unit =
    if head == None then
      head = Some(node)
      tail = Some(node)
    else
      node.next = head
      head.foreach(_.prev = Some(node))
      head = Some(node)

  private def trimCache(): Unit =
    if length > capacity then
      tail.foreach: node =>
        detach(node)
        lookup.remove(node.key)
        length -= 1

object LRU:
  def empty[K, V](capacity: Int = 10): LRU[K, V] = LRU(capacity)
