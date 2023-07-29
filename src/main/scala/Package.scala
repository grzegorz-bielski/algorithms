package algos

import scala.collection.mutable.ArrayBuffer

type Id[A] = A

extension [T](a: Array[T])
  def swap(i: Int, j: Int): Array[T] =
    val temp = a(i)
    a(i) = a(j)
    a(j) = temp
    a

  def swap_(i: Int, j: Int): Unit =
    swap(i, j)
    ()

  def middle: T =
    val mid = Math.floor(a.length / 2).toInt
    a(mid)

extension [T](a: ArrayBuffer[T])
  def swap(i: Int, j: Int): ArrayBuffer[T] =
    val temp = a(i)
    a(i) = a(j)
    a(j) = temp
    a

  def swap_(i: Int, j: Int): Unit =
    swap(i, j)
    ()

  def middle: T =
    val mid = Math.floor(a.length / 2).toInt
    a(mid)
