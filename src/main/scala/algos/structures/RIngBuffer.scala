package algos.structures

import scala.reflect.ClassTag
import java.util.concurrent.atomic.AtomicInteger

// https://www.horsedrawngames.com/ring-buffers-circular-buffers/
// https://aquarchitect.github.io/swift-algorithm-club/Ring%20Buffer/

/** Single producer / single producer ring buffer. Not thread safe for multiple producers / consumers.
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

final class RingBuffer2[T: ClassTag](size: Int):
  private val array = Array.ofDim[T](size)

  private val readIndex = AtomicInteger(0)
  private val writeIndex = AtomicInteger(0)

  private def readingSpace: Int = writeIndex.get - readIndex.get
  private def writingSpace: Int = size - readingSpace

  def isFull: Boolean = writingSpace == 0
  def isEmpty: Boolean = readingSpace == 0

  def push(a: T): Boolean =
    if isFull then false
    else
      val prev = writeIndex.getAndIncrement()
      array(prev % size) = a
      true

  def pop: Option[T] =
    if isEmpty then None
    else
      val prev = readIndex.getAndIncrement()
      val a = array(prev % size)
      Some(a)
