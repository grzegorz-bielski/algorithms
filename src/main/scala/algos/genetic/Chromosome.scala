package algos.genetic

import scala.collection.immutable.ArraySeq
import scala.util.Random

trait Chromosome[T <: Chromosome[T]]:
  def fitness: Double
  def crossover(other: T)(using Random): ArraySeq[T]
  def mutate(using Random): T

object Chromosome:
  given Ordering[Chromosome[?]] = Ordering.by(_.fitness)
