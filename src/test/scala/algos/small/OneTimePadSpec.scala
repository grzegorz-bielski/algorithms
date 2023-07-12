package algos.small

import org.scalatest.*
import flatspec.*
import matchers.*

class OneTimePadSpec extends AnyFlatSpec, should.Matchers:
  "encrypt/decrypt" should "roundtrip correctly" in:
    val msg = "One Time Pad uh oh"

    OneTimePad.decrypt(OneTimePad.encrypt(msg)) shouldEqual msg
