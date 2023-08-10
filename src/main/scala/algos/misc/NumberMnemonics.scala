package algos.misc

object PhoneNumberMnemonics:
  val phoneMapping = Map(
    '1' -> Array("1"),
    '2' -> Array("A", "B", "C"),
    '3' -> Array("D", "E", "F"),
    '4' -> Array("G", "H", "I"),
    '5' -> Array("J", "K", "L"),
    '6' -> Array("M", "N", "O"),
    '7' -> Array("P", "Q", "R", "S"),
    '8' -> Array("T", "U", "V"),
    '9' -> Array("W", "X", "Y", "Z"),
    '0' -> Array("0")
  )

  @main def phoneMnemonics = println(mnemonics("1440787").mkString("\n"))

  def mnemonics(phoneNumber: String): Array[String] =
    phoneNumber
      .map(phoneMapping)
      .foldLeft(Array(""))(cartesianProduct)

  def cartesianProduct(a: Array[String], b: Array[String]): Array[String] =
    for
      x <- a
      y <- b
    yield x + y
