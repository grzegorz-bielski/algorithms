package algos.neural

object Normalization:
  def byFeatureScaling(data: Array[Array[Double]]): Array[Array[Double]] =
    val minMax = data.transpose.map(col => (col.min, col.max))

    data.map: row =>
      row
        .zip(minMax)
        .map:
          case (value, (min, max)) => (value - min) / (max - min)
