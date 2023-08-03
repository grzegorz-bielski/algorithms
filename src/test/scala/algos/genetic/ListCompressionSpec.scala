package algos.genetic

import org.scalatest.*, funsuite.*, matchers.*

class ListCompressionSpec extends AnyFunSuite, should.Matchers:
  test("compresses the list"):
    (
      for
        compressed <- listCompression(toCompress).bytesCompressed
        original <- ListCompression(toCompress).bytesCompressed
      yield assert(compressed < original)
    ).getOrElse(fail("Failed to compress"))
