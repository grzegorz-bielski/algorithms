package algos.structures

import HashMap.*

/** An immutable Hash Table implementation
  */
final class HashMap[K, V] private (entries: Vector[Vector[Entry[K, V]]], val size: Int, growFactor: Double):
  def add(key: K, value: V): HashMap[K, V] =
    val i = index(key)
    val entry = Entry(key, value)

    // grow if needed
    // TODO: should also handle shrinking
    val nextEntries = if loadFactor >= growFactor then entries.appended(Vector.empty) else entries

    val mapped = nextEntries(i)

    HashMap(
      entries = mapped.indexWhere(_.key == key, from = 0) match
        case -1 => nextEntries.updated(i, entry +: mapped)
        case j  => nextEntries.updated(i, mapped.updated(j, entry))
      ,
      size = size + 1,
      growFactor = growFactor
    )

  def remove(key: K): HashMap[K, V] =
    val i = index(key)

    val mapped = entries(i)

    HashMap(
      entries = entries.updated(i, mapped.filterNot(_.key == key)),
      size = size - 1,
      growFactor = growFactor
    )

  def get(key: K): Option[V] =
    entries(index(key)).collectFirst:
      case Entry(k, v) if k == key => v

  private def index(key: K): Int = key.hashCode() % entries.length

  // The amount of data points vs the amount of storage -> data.len / storage.capacity
  private def loadFactor: Double = size.toDouble / entries.length

object HashMap:
  def empty[K, V](initialCapacity: Int = 6, growFactor: Double = 0.8): HashMap[K, V] =
    HashMap(
      entries = Vector.fill(initialCapacity)(Vector.empty),
      size = 0,
      growFactor = growFactor
    )

  final case class Entry[K, V](key: K, value: V)

  extension [K, V](entries: Vector[Entry[K, V]])
    def chainIndex(key: K): Int = entries.indexWhere(_.key == key, from = 0)
