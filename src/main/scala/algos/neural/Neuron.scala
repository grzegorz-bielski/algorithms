package algos.neural

import algos.*

// mutable neuron, not thread safe
final class Neuron(
    val weights: Array[Double],
    val learningRate: Double,
    val activationFn: Double => Double,
    val derivativeActivationFn: Double => Double
):
  private var currentDelta: Double = 0

  def delta: Double = currentDelta
  def delta_=(newDelta: Double): Unit = currentDelta = newDelta

  private var outputCache: Double = 0

  def cachedOutput: Double = outputCache

  def output(inputs: Array[Double]): Double =
    outputCache = inputs dotProduct weights
    activationFn(outputCache)
