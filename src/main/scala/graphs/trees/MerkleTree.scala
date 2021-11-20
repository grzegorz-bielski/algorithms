package graphs.trees

import java.util.Base64

/** MerkleTree represents a binary hash tree in which a:
  * - leaf node contains a hash of the data block
  * - non-leaf node contains a hash of its child nodes hashes
  */
class MerkleTree(
    val hash: Array[Byte],
    val left: Option[MerkleTree] = None,
    val right: Option[MerkleTree] = None
) extends Serializable:

  override def toString =
    s"MT(hash = ${printHash(hash)})"

  private def printHash(hash: Array[Byte]) =
    Base64.getEncoder.nn.encodeToString(hash)

object MerkleTree:
  def sha256 =
    import java.security.MessageDigest
    MerkleTree(MessageDigest.getInstance("SHA-256").nn.digest(_).nn)

  /** @param fn hashing functions
    * @param data an array of data blocks for leaf nodes
    */
  def apply(fn: Array[Byte] => Array[Byte])(data: Array[Array[Byte]]) =
    if data.isEmpty then
      new MerkleTree(fn(Array.empty[Byte]))
    else
      unfold(fn, data.map(fn.andThen(new MerkleTree(_))))(0)

  @scala.annotation.tailrec
  private def unfold(
      fn: Array[Byte] => Array[Byte],
      nodes: Array[MerkleTree]
  ): Array[MerkleTree] =
    nodes match
      case ns if ns.size <= 1 => ns
      case ns =>
        unfold(
          fn,
          ns.grouped(2)
            .map {
              case Array(a, b) =>
                new MerkleTree(fn(a.hash ++ b.hash), Some(a), Some(b))
              case Array(a) => new MerkleTree(fn(a.hash), Some(a), None)
            }
            .toArray
        )

