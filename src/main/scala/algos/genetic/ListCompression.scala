package algos.genetic

import algos.*
import java.io.*
import java.util.zip.*

import scala.util.Try
import scala.util.Random
import scala.collection.immutable.ArraySeq

// format: off
val toCompress = ArraySeq(
    "Michael", "Sarah", "Joshua", "Narine", "David", "Sajid", "Melanie", "Daniel", "Wei", "Dean", "Brian", "Murat", "Lisa"
)
// format: on

@main def listCompressionApp =
  val compressed = listCompression(toCompress)

  println(s"Original size: ${ListCompression(toCompress).bytesCompressed}")
  println(s"Compressed size: ${compressed.bytesCompressed}")

/** Reshuffles the array elements until they have smallest compressed size
  */
def listCompression[T](toCompress: ArraySeq[T]) =
  given Random = Random

  geneticAlgorithm(
    initialPopulation = Vector.fill(100)(ListCompression.random(toCompress)),
    mutationChance = 0.2,
    crossoverChance = 0.7,
    maxGenerations = 100,
    fitnessThreshold = 1,
    selectionType = SelectionType.Tournament
  )

final case class ListCompression[T](xs: ArraySeq[T]) extends Chromosome[ListCompression[T]]:
  def fitness: Double =
    bytesCompressed.map(1.0 / _).getOrElse(0.0)

  def mutate(using random: Random): ListCompression[T] =
    val i = random.nextInt(xs.size)
    val j = random.nextInt(xs.size)

    ListCompression(xs.swap(i, j))

  def crossover(other: ListCompression[T])(using random: Random): ArraySeq[ListCompression[T]] =
    val thisI = random.nextInt(xs.size)
    val otherI = random.nextInt(other.xs.size)

    val thisElement = xs(thisI)
    val otherElement = other.xs(otherI)

    val thisJ = xs.indexOf(otherElement)
    val otherJ = other.xs.indexOf(thisElement)

    ArraySeq(xs.swap(thisI, thisJ), other.xs.swap(otherI, otherJ)).map(ListCompression(_))

  lazy val bytesCompressed: Option[Int] =
    Try:
      val buffer = ByteArrayOutputStream()
      val oos = ObjectOutputStream(GZIPOutputStream(buffer))

      oos.writeObject(xs)
      oos.close()

      buffer.size()
    .toOption

  override def toString(): String = s"xs: $xs, bytes: $bytesCompressed"
object ListCompression:
  def random[T](xs: ArraySeq[T])(using random: Random): ListCompression[T] =
    ListCompression(random.shuffle(xs))
