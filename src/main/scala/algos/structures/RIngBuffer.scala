package algos.structures

import scala.reflect.ClassTag

// https://www.horsedrawngames.com/ring-buffers-circular-buffers/
// https://aquarchitect.github.io/swift-algorithm-club/Ring%20Buffer/

/** Lock free single producer, single consumer ring buffer.
  *
  * For multiple producers / consumers the access to the underlying array should synchronized by locks.
  */
final class RingBuffer[T: ClassTag](size: Int):
  private val array = Array.ofDim[T](size)

  @volatile private var readIndex = 0
  @volatile private var writeIndex = 0

  private def readingSpace: Int = writeIndex - readIndex
  private def writingSpace: Int = size - readingSpace

  def isFull: Boolean = writingSpace == 0
  def isEmpty: Boolean = readingSpace == 0

  def push(a: T): Boolean =
    if isFull then false
    else
      array(writeIndex % size) = a
      writeIndex = writeIndex + 1
      true

  def pop: Option[T] =
    if isEmpty then None
    else
      val a = array(readIndex % size)
      readIndex = readIndex + 1
      Some(a)
