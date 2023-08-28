package algos.structures

import java.util.concurrent.locks.ReentrantLock
import scala.reflect.ClassTag
import scala.language.unsafeNulls

/** Thread safe blocking ring buffer. Essentially a `ArrayBlockingQueue`
  */
final class BlockingRingBuffer[T: ClassTag](size: Int):
  private val array = Array.ofDim[T](size)
  private var readIndex = 0
  private var writeIndex = 0

  private val lock = ReentrantLock()
  private val notFull = lock.newCondition()
  private val notEmpty = lock.newCondition()

  private def readingSpace: Int = writeIndex - readIndex
  private def writingSpace: Int = size - readingSpace

  private def isFull: Boolean = writingSpace == 0
  private def isEmpty: Boolean = readingSpace == 0

  def push(a: T): Unit =
    lock.lock()
    try
      while isFull do notFull.await()
      array(writeIndex % size) = a
      writeIndex += 1
      notEmpty.signal()
    finally lock.unlock()

  def pop(): T =
    lock.lock()
    try
      while isEmpty do notEmpty.await()
      val a = array(readIndex % size)
      readIndex += 1
      notFull.signal()
      a
    finally lock.unlock()
