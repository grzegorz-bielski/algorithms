package algos.genetic

import scala.collection.immutable.ArraySeq
import scala.util.Random

@main def geneticEquationSolverApp = println(geneticEquationSolver)

def geneticEquationSolver =
  given Random = Random

  geneticAlgorithm(
    initialPopulation = Vector.fill(20)(Equation.random),
    mutationChance = 0.1,
    crossoverChance = 0.7,
    maxGenerations = 100,
    fitnessThreshold = 13,
    selectionType = SelectionType.Tournament
  )

final case class Equation(x: Double, y: Double) extends Chromosome[Equation]:
  def fitness: Double = 6 * x - x * x + 4 * y - y * y

  def crossover(other: Equation)(using Random): ArraySeq[Equation] =
    ArraySeq(this.copy(y = other.y), other.copy(y = this.y))

  def mutate(using random: Random): Equation =
    if random.nextDouble() > 0.5 then copy(x = if random.nextDouble() > 0.5 then x + 1 else x - 1)
    else copy(y = if random.nextDouble() > 0.5 then y + 1 else y - 1)

  override def toString(): String = s"x = $x, y = $y, fitness = $fitness"

object Equation:
  val maxStart = 100

  def random(using random: Random): Equation = Equation(random.nextInt(maxStart), random.nextInt(maxStart))
