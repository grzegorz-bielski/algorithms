package algos.genetic

import scala.util.Random
import scala.collection.mutable.ArrayBuffer

enum SelectionType:
  case Tournament
  case RouletteWheel

// TODO: a concurrent version

/** A genetic algorithms framework
  *
  * @param initialPopulation
  *   The chromosomes in the first generation
  * @param mutationChance
  *   The chance that each chromosome in each generation will mutate
  * @param crossoverChance
  *   The chance that the children will have a mixture of their parents' genes, rather than being clones
  * @param maxGenerations
  *   The maximum number of generations to run the algorithm for
  * @param fitnessThreshold
  *   The fitness value at which the algorithm will stop
  * @param selectionType
  *   The type of selection to use when choosing which chromosomes will reproduce
  * @param random
  *   The random number generator to use
  */
def geneticAlgorithm[C <: Chromosome[C]](
    initialPopulation: Vector[C],
    mutationChance: Double,
    crossoverChance: Double,
    maxGenerations: Int,
    fitnessThreshold: Double,
    selectionType: SelectionType
)(using random: Random): C =
  val selection = Selection[C](selectionType)
  var population: ArrayBuffer[C] = initialPopulation to ArrayBuffer

  // mutates `population`
  def run(): C =
    @scala.annotation.tailrec
    def go(best: C, gen: Int): C =
      if gen > maxGenerations || best.fitness >= fitnessThreshold then best
      else
        reproduce()
        mutate()
        val candidate = population.max
        val newBest = if candidate.fitness > best.fitness then candidate else best

        go(newBest, gen + 1)
    go(population.max, 1)

  // mutates `population`
  def mutate(): Unit =
    population = population.map: individual =>
      if random.nextDouble() < mutationChance then individual.mutate else individual

  // mutates `population`
  def reproduce(): Unit =
    @scala.annotation.tailrec
    def go(nextPopulation: ArrayBuffer[C]): ArrayBuffer[C] =
      if nextPopulation.size >= population.size then nextPopulation.take(population.size)
      else
        val parents = selection.select(picks = 2, population)

        val toAdd =
          if random.nextDouble() < crossoverChance then
            (for
              one <- parents.lift(0)
              two <- parents.lift(1)
            yield one crossover two).getOrElse(parents)
          else parents

        go(nextPopulation ++= toAdd)

    population = go(ArrayBuffer.empty)

  run()
end geneticAlgorithm

final class Selection[C <: Chromosome[C]](selectionType: SelectionType)(using random: Random):
  def select(picks: Int, population: ArrayBuffer[C]): ArrayBuffer[C] =
    selectionType match
      case SelectionType.Tournament =>
        selectTournament(participants = population.size / 2, picks = picks, population)
      case SelectionType.RouletteWheel =>
        val totalFitness = population.map(_.fitness).sum
        val wheel = population.map(_.fitness / totalFitness)

        selectRoulette(wheel = wheel, picks = picks, population)

  /** Selection type based on each chromosome's proportion of fitness to the sum of all fitness values in a generation
    *
    * @param wheel
    *   an array with each chromosome percentage of total fitness
    * @param numOfPick
    *   number of chromosomes to pick
    * @return
    *   picked chromosomes
    */
  private def selectRoulette(wheel: ArrayBuffer[Double], picks: Int, population: ArrayBuffer[C]): ArrayBuffer[C] =
    (0 to picks).foldLeft(ArrayBuffer.empty[C]): (acc, _) =>
      var pick = random.nextDouble()
      // spin until the sum of traversed fitnesses fraction is greater than the random number
      wheel.indices
        .find: i =>
          pick -= wheel(i)
          pick <= 0
        .foreach: i =>
          acc += population(i)
      acc

  /** Take a number of participants at random and select the best of them
    *
    * @param participants
    *   number of participants in the tournament
    * @param picks
    *   number of chromosomes to pick from the winners
    * @return
    *   the new generation
    */
  private def selectTournament(participants: Int, picks: Int, population: ArrayBuffer[C]): ArrayBuffer[C] =
    random
      .shuffle(population)
      .view
      .take(participants)
      .sorted(using Ordering[Chromosome[?]].reverse)
      .take(picks)
      .to(ArrayBuffer)
