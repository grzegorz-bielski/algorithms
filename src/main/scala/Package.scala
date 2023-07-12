package algos

extension [T](a: Array[T])
  def swap(i: Int, j: Int): Array[T] =
    val temp = a(i)
    a(i) = a(j)
    a(j) = temp
    a

  def swap_(i: Int, j: Int): Unit =
    swap(i, j)
    ()

  def middle =
    val mid = Math.floor(a.length / 2).toInt
    a(mid)
