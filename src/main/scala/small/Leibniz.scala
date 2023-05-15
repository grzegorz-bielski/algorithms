package small

// PI = 4/1 - 4/3 + 4/5 - 4/7 + 4/9 - 4/11 ...
object Leibniz:
  def pi(n: Int): Double =
    (1 to n).foldLeft(0.0): (acc, i) =>
      val sign = if i % 2 == 0 then -1 else 1
      acc + sign * 4.0 / (2 * i - 1)
