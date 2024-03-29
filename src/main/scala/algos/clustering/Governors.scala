package algos.clustering

import Governor.*
import scala.util.Random

final case class Governor(dimensions: Vector[Double], originals: Vector[Double], state: String) extends DataPoint:
  type P = Governor

  def longitude = originals(0)
  def age = originals(1)

  def withDimensions(newDimensions: Vector[Double]): P = copy(dimensions = newDimensions)

  override def toString(): String = s"$state: (longitude: $longitude, age: $age)"

object Governor:
  def fromTuple(t: (Double, Int, String)): Governor = apply(t._1, t._2.toDouble, t._3)
  def apply(longitude: Double, age: Double, state: String): Governor =
    val dims = Vector(longitude, age)
    Governor(dims, dims, state)

object Governors:
  val all = Vector(
    (-86.79113, 72, "Alabama"),
    (-152.404419, 66, "Alaska"),
    (-111.431221, 53, "Arizona"),
    (-92.373123, 66, "Arkansas"),
    (-119.681564, 79, "California"),
    (-105.311104, 65, "Colorado"),
    (-72.755371, 61, "Connecticut"),
    (-75.507141, 61, "Delaware"),
    (-81.686783, 64, "Florida"),
    (-83.643074, 74, "Georgia"),
    (-157.498337, 60, "Hawaii"),
    (-114.478828, 75, "Idaho"),
    (-88.986137, 60, "Illinois"),
    (-86.258278, 49, "Indiana"),
    (-93.210526, 57, "Iowa"),
    (-96.726486, 60, "Kansas"),
    (-84.670067, 50, "Kentucky"),
    (-91.867805, 50, "Louisiana"),
    (-69.381927, 68, "Maine"),
    (-76.802101, 61, "Maryland"),
    (-71.530106, 60, "Massachusetts"),
    (-84.536095, 58, "Michigan"),
    (-93.900192, 70, "Minnesota"),
    (-89.678696, 62, "Mississippi"),
    (-92.288368, 43, "Missouri"),
    (-110.454353, 51, "Montana"),
    (-98.268082, 52, "Nebraska"),
    (-117.055374, 53, "Nevada"),
    (-71.563896, 42, "New Hampshire"),
    (-74.521011, 54, "New Jersey"),
    (-106.248482, 57, "New Mexico"),
    (-74.948051, 59, "New York"),
    (-79.806419, 60, "North Carolina"),
    (-99.784012, 60, "North Dakota"),
    (-82.764915, 65, "Ohio"),
    (-96.928917, 62, "Oklahoma"),
    (-122.070938, 56, "Oregon"),
    (-77.209755, 68, "Pennsylvania"),
    (-71.51178, 46, "Rhode Island"),
    (-80.945007, 70, "South Carolina"),
    (-99.438828, 64, "South Dakota"),
    (-86.692345, 58, "Tennessee"),
    (-97.563461, 59, "Texas"),
    (-111.862434, 70, "Utah"),
    (-72.710686, 58, "Vermont,"),
    (-78.169968, 60, "Virginia"),
    (-121.490494, 66, "Washington"),
    (-80.954453, 66, "West Virginia"),
    (-89.616508, 49, "Wisconsin"),
    (-107.30249, 55, "Wyoming")
  ).map(Governor.fromTuple)

  def clusters(using Random) = KMeans.run(
    k = 2,
    rawPoints = all,
    maxIterations = 100
  )
