package algos.clustering

import java.util.DoubleSummaryStatistics

final case class Statistics private (samples: Vector[Double], summary: DoubleSummaryStatistics):
  lazy val mean = summary.getAverage
  lazy val sum = summary.getSum

  // sum((Xi - mean)^2) / N
  lazy val variance =
    samples.view.map(x => math.pow(x - mean, 2)).sum / samples.size

  lazy val std = math.sqrt(variance)

  // standard score
  // (Xi - mean) / std
  lazy val zScored = samples.map(x => (x - mean) / std)

  lazy val max = summary.getMax
  lazy val min = summary.getMin

object Statistics:
  def fromSamples(samples: Vector[Double]): Statistics =
    val summary = samples.foldLeft(DoubleSummaryStatistics()): (acc, x) =>
      acc.accept(x)
      acc

    Statistics(samples, summary)

  extension (samples: Vector[Double])
    def asStats: Statistics = Statistics.fromSamples(samples)
