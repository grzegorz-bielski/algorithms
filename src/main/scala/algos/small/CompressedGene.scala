package algos.small

// 1 bit: 2 possible values
// 2 bits: 4 possible values
// 64 bits: 2^64 possible values

// bit string: arbitrary-length sequence of bits

// Vector(A  T  G)
//        00 01 10
//        000110 - underlying representation

// import scala.collection.immutable.BitSet
import java.util.BitSet

enum Nucleotide:
  case A, C, G, T

final case class CompressedGene(bitSet: BitSet, size: Int)
object CompressedGene:
  def compress(gene: Vector[Nucleotide]): CompressedGene =
    val size = gene.size
    val bitSet = BitSet(size * 2)

    for i <- 0 until size do
      val (first, second) = (2 * i, 2 * i + 1)
      gene(i) match
        case Nucleotide.A => // 00｀
          bitSet.set(first, false)
          bitSet.set(second, false)
        case Nucleotide.C => // 01
          bitSet.set(first, false)
          bitSet.set(second, true)
        case Nucleotide.G => // 10
          bitSet.set(first, true)
          bitSet.set(second, false)
        case Nucleotide.T => // 11
          bitSet.set(first, true)
          bitSet.set(second, true)

    CompressedGene(bitSet, size)

  def decompress(gene: CompressedGene): Vector[Nucleotide] =
    (0 until gene.size * 2).foldLeft(Vector.empty[Nucleotide]):
      // process 2 bits at a time
      case (acc, i) if i % 2 == 0 =>
        val (first, second) = (gene.bitSet.get(i).toInt, gene.bitSet.get(i + 1).toInt)

        // (1, 0) G
        // (0, 0) A
        // (0, 1) C
        // (1, 1) T
        
        // G: 1 << 1 | 0
        // G: 10 | 0
        // G: 10

        // A: 0 << 1 | 0
        // A: 0 | 0
        // A: 0

        // C: 0 << 1 | 1
        // C: 0 | 1
        // C: 1
        
        // T: 1 << 1 | 1
        // T: 10 | 1
        // T: 11
        val lastBits = first << 1 | second

        acc :+ (lastBits match
          // 00
          case 0 => Nucleotide.A
          // 01
          case 1 => Nucleotide.C
          // 10
          case 2 => Nucleotide.G
          // 11
          case 3 => Nucleotide.T
        )
      case (acc, _) => acc

extension (underlying: Boolean) def toInt: Int = if underlying then 1 else 0
