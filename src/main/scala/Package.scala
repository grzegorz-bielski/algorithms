package algos

import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.ArraySeq

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

extension [T](a: ArraySeq[T])
  def swap(i: Int, j: Int): ArraySeq[T] =
    a.updated(i, a(j)).updated(j, a(i))

  def middle: T =
    val mid = Math.floor(a.length / 2).toInt
    a(mid)
