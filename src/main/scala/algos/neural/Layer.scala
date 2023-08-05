package algos.neural

import scala.util.Random
import algos.*

final class Layer(
    val previousLayer: Option[Layer],
    val neurons: Array[Neuron],
    private var outputCache: Array[Double]
):
  def cachedOutput: Array[Double] = outputCache

  def outputs(inputs: Array[Double]): Array[Double] =
    // every neuron in the layer receives the signal from every neuron in the previous layer
    // a pass-through for input layer
    outputCache = previousLayer.fold(inputs)(_.neurons.map(_.output(inputs)))
    outputCache

  // back-propagation

  // output layer only
  def calculateDeltasForOutputLayer(expected: Array[Double]): Unit =
    for
      i <- neurons.indices
      e <- expected.lift(i)
      c <- cachedOutput.lift(i)
      neuron = neurons(i)
      nextDelta = neuron.derivativeActivationFn(neuron.cachedOutput) * (e - c)
    yield neuron.delta = nextDelta
    ()

  def updateWeights(): Unit =
    for
      neuron <- neurons
      wi <- neuron.weights.indices
      cached <- previousLayer.flatMap(_.cachedOutput.lift(wi))
    yield neuron.weights(wi) += neuron.learningRate * cached * neuron.delta
    ()

// hidden layers only
  def calculateDeltasForHiddenLayer(nextLayer: Layer): Unit =
    neurons.indices.foreach: i =>
      val neuron = neurons(i)
      val nextWeights = nextLayer.neurons.map(_.weights.lift(i).getOrElse(0d))
      val nextDeltas = nextLayer.neurons.map(_.delta)
      val sum = nextWeights dotProduct nextDeltas
      val nextDelta = neuron.derivativeActivationFn(neuron.cachedOutput) * sum

      neuron.delta = nextDelta

object Layer:
  def create(
      previousLayer: Option[Layer],
      neuronsNumber: Int,
      learningRate: Double,
      activationFn: Double => Double,
      derivativeActivationFn: Double => Double
  )(using random: Random) =
    val neurons: Array[Neuron] = Array.fill(neuronsNumber):
      val weights = previousLayer.fold(Array.empty[Double]):
        _.neurons.map(_ => random.nextDouble())

      Neuron(weights, learningRate, activationFn, derivativeActivationFn)

    val outputCache = Array.ofDim[Double](neuronsNumber)

    Layer(previousLayer, neurons, outputCache)
