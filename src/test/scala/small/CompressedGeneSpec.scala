package small

import org.scalatest.*, prop.*
import flatspec.*
import matchers.*

import scala.collection.immutable.BitSet

class CompressedGeneSpec extends AnyFlatSpec, should.Matchers:
  "compress/decompress" should "roundtrip correctly" in:
    val nucleotides = Vector(Nucleotide.G, Nucleotide.A, Nucleotide.C, Nucleotide.T)

    CompressedGene.decompress(CompressedGene.compress(nucleotides)) shouldEqual nucleotides
