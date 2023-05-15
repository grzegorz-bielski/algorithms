package small

import java.nio.charset.Charset
import scala.util.Random

// encrypted = og ^ dummy
// og = encrypted ^ dummy

// symmetric-key encryption 
object OneTimePad:
  def encrypt(data: String): KeyPair =
    val og = Key.fromString(data)
    val dummy = Key.pseudoRandomKey(og.length) // effectful
    val encrypted = Array.ofDim[Byte](og.length)

    for i <- 0 until og.length do encrypted(i) = (og(i) ^ dummy(i)).toByte

    KeyPair(dummy, encrypted)

  def decrypt(kp: KeyPair): String =
    val decrypted = Array.ofDim[Byte](kp.encrypted.length)

    for i <- 0 until kp.encrypted.length do decrypted(i) = (kp.encrypted(i) ^ kp.dummy(i)).toByte

    String(decrypted, charset)

  private val charset = Charset.forName("UTF-8")

  type Key = Array[Byte]
  object Key:
    def fromString(s: String): Key = s.getBytes(charset).nn
    def pseudoRandomKey(length: Int): Key = Random.nextBytes(length)

  /** One-time pad encryption key pair
    * @param dummy
    *   pseudo-random dummy data
    * @param encrypted
    *   product of dummy data and original data
    */
  final case class KeyPair(dummy: Key, encrypted: Key)
