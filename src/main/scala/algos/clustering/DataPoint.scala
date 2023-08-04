package algos.clustering

trait DataPoint:
  type P <: DataPoint

  def dimensions: Vector[Double]

  def originals: Vector[Double]

  def withDimensions(newDimensions: Vector[Double]): P

  def distance(other: DataPoint): Double =
    // euclidean distance for n dimensions
    // sqrt(sum((Xi - Yi)^2))
    math.sqrt:
      dimensions.view
        .zip(other.dimensions)
        .map((x, y) => math.pow(x - y, 2))
        .sum
