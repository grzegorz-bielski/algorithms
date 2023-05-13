package small

import org.scalatest.*, prop.*
import flatspec.*
import matchers.*

import scala.collection.immutable.BitSet

class OneTimePadSpec extends AnyFlatSpec, should.Matchers:
  "encrypt/decrypt" should "roundtrip correctly" in:
    val msg = "One Time Pad uh oh"

    OneTimePad.decrypt(OneTimePad.encrypt(msg)) shouldEqual msg
