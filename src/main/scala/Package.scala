package algos

extension [T](a: Array[T])
  def swap(i: Int, j: Int): Array[T] =
    val iv = a(i)
    val jv = a(j)
    a(i) = jv
    a(j) = iv
    a

  def middle =
    val mid = Math.floor(a.length / 2).toInt
    a(mid)
