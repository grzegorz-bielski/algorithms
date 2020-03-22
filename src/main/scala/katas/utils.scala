package katas

object Utils {
  def findOdd(xs: Seq[Int]): Int =
    xs.groupMapReduce(identity)(_ => 1)(_ + _).find(_._2 % 2 != 0).map(_._1).get

  def vowelCount(inputStr: String): Int = {
    val vowels = Seq("a", "e", "i", "o", "u")
    inputStr
      .split("")
      .foldLeft(0)((acc, curr) => if (vowels.contains(curr)) acc + 1 else acc)
  }

}
