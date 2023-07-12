package algos.aoc

object PasswordPhilosophy:
  def countValidPasswords = traversePasswords:
    case (min, max, char, pass) =>
      val n = pass.count(_ == char)
      n > max || n < min
  

  def countValidPasswords2 = traversePasswords:
    case (posA, posB, char, pass) =>
      val isOnA = pass.charAt(posA - 1) == char
      val isOnB = pass.charAt(posB - 1) == char

      isOnB == isOnA

  private def traversePasswords(fn: ((Int, Int, Char, String)) => Boolean) =
    io.Source
      .fromResource("passwordPhilosophyInput.txt")
      .getLines
      .foldLeft(0)((z, line) => if fn(parseLine(line)) then z else z + 1)

  private def parseLine(line: String) =
    import scala.language.unsafeNulls

    val space = " " `charAt` 0
    val firstSpace = line `indexOf` space
    val secondSpace = line `lastIndexOf` space

    val char = line `charAt` secondSpace - 2
    val pass = line drop secondSpace + 1

    val Array(firstNum, secondNum) = line
      .slice(0, firstSpace)
      .split("-")
      .map(_.toInt)

    (firstNum, secondNum, char, pass)

