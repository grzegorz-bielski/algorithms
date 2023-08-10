package algos.misc

import org.scalatest.*
import flatspec.*
import matchers.*

class CompressedGeneSpec extends AnyFlatSpec, should.Matchers:
  "compress/decompress" should "roundtrip correctly" in:
    val nucleotides = Vector(Nucleotide.G, Nucleotide.A, Nucleotide.C, Nucleotide.T)

    CompressedGene.decompress(CompressedGene.compress(nucleotides)) shouldEqual nucleotides
