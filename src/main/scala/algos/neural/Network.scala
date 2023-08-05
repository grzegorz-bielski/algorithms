package algos.neural

import scala.util.Random

final class Network[T](layers: Array[Layer]):
  def train(inputs: Array[Array[Double]], expecteds: Array[Array[Double]]): Unit =
    inputs
      .zip(expecteds)
      .foreach: (input, expected) =>
        outputs(input)
        backpropagate(expected)
        updateWeights()

  def predict(inputs: Array[Double], fn: Array[Double] => T): T = fn(outputs(inputs))

  final case class ValidationResult(correct: Int, trials: Int, percentage: Double)

  def validate(inputs: Array[Array[Double]], expecteds: Array[Double], fn: Array[Double] => T): ValidationResult =
    val correct = inputs
      .zip(expecteds)
      .foldLeft(0):
        case (acc, (input, expected)) =>
          if predict(input, fn) == expected then acc + 1 else acc

    val percentage = correct.toDouble / inputs.length

    ValidationResult(correct, inputs.length, percentage)

  private def outputs(inputs: Array[Double]): Array[Double] =
    layers.foldLeft(inputs)((prevInputs, layer) => layer.outputs(prevInputs))

  private def backpropagate(expected: Array[Double]): Unit =
    layers.last.calculateDeltasForOutputLayer(expected)
    layers.indices.reverseIterator
      .drop(1)
      .foreach: i =>
        layers(i).calculateDeltasForHiddenLayer(layers(i + 1))

  private def updateWeights(): Unit =
    layers.drop(1).foreach(_.updateWeights())

object Network:
  def create[T](
      layerStructure: List[Int],
      learningRate: Double,
      activationFn: Double => Double = sigmoid,
      derivativeActivationFn: Double => Double = derivativeSigmoid
  )(using Random): Either[String, Network[T]] =
    if layerStructure.size < 3 then Left("There should be at least 3 layers")
    else
      Right:
        val layerOf = Layer.create(_, _, learningRate, activationFn, derivativeActivationFn)
        val inputLayer = layerOf(None, layerStructure.head)
        val layers = (1 to layerStructure.size).foldLeft(Array(inputLayer)): (acc, i) =>
          acc :+ layerOf(Some(acc.last), layerStructure(i))

        Network(layers)
