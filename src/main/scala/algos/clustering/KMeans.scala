package algos.clustering

import scala.collection.immutable.Range
import scala.util.Random
import Statistics.*

//  K-means clustering algorithm that groups data points into predefined number of clusters
// uses random centroids for initialization, see k-means++ for better initialization

// Steps:
// 1. Normalize all the data points
// 2. Initialize `k` empty clusters and create random centroids associated with each cluster
// 3. Assign each data point to the cluster with the closest centroid
// 4. Update the centroid of each cluster by calculating the average of all the data points in that cluster
// 5. Repeat steps 3 and 4 until the centroids don't change or the maximum number of iterations is reached

object KMeans:
  def run(k: Int, rawPoints: Vector[DataPoint], maxIterations: Int)(using Random): Vector[Cluster] =
    val points = normalize(rawPoints) // step 1
    val initialClusters = initializeClusters(k, points) // step 2

    @scala.annotation.tailrec
    def go(i: Int, clusters: Vector[Cluster]): Vector[Cluster] =
      if i >= maxIterations then clusters
      else
        val purgedClusters = clusters.map(_.purgePoints) // remove points from prev iteration
        val assignedClusters = assignClusters(points, purgedClusters) // step 3
        val nextClusters = generateCentroids(assignedClusters) // step 4

        // step 5
        if nextClusters == clusters then nextClusters
        else go(i + 1, nextClusters)

    go(0, initialClusters)

  // step 1.
  private def normalize(points: Vector[DataPoint]): Vector[DataPoint] =
    val normalizedColumns =
      points.dimensionIndices.map(points.column(_).toStats.zScored)

    points.zipWithIndex.map: (p, pi) =>
      p.withDimensions(p.dimensions.indices.map(normalizedColumns(_)(pi)).toVector)

  // step 2.
  private def initializeClusters(k: Int, points: Vector[DataPoint])(using random: Random): Vector[Cluster] =
    def randomDimensions(): Vector[Double] =
      points.dimensionIndices.foldLeft(Vector.empty): (acc, i) =>
        val stats = points.column(i).toStats

        acc :+ random.between(stats.min, stats.max)

    (0 until k).foldLeft(Vector.empty[KMeans.Cluster]): (acc, _) =>
      points.headOption.fold(acc): point =>
        acc :+ Cluster(Vector.empty, point.withDimensions(randomDimensions()))

  // step 3. Find closes cluster centroid for each point and assign it to that cluster
  private def assignClusters(points: Vector[DataPoint], clusters: Vector[Cluster]): Vector[Cluster] =
    points.foldLeft(clusters): (acc, point) =>
      acc.indices
        .minByOption(i => point.distance(acc(i).centroid))
        .map(i => acc.updated(i, acc(i).withPoint(point)))
        .getOrElse(acc)

    // step 4.
  private def generateCentroids(clusters: Vector[Cluster]): Vector[Cluster] =
    clusters.map: cluster =>
      if cluster.points.isEmpty then cluster
      else
        cluster.adaptCentroid:
          cluster.points.dimensionIndices.map(cluster.points.column(_).toStats.mean).toVector

  final case class Cluster(points: Vector[DataPoint], centroid: DataPoint):
    def purgePoints: Cluster = copy(points = Vector.empty)
    def withPoint(point: DataPoint): Cluster = copy(points = points :+ point)
    def adaptCentroid(dimensions: Vector[Double]) =
      copy(centroid = centroid.withDimensions(dimensions))

  extension (points: Vector[DataPoint])
    def dimensionsCount: Int = points.headOption.map(_.dimensions.size).getOrElse(0)
    def dimensionIndices: Range = 0 until dimensionsCount
    def column(dimension: Int) = points.flatMap(_.dimensions.lift(dimension).toVector)
