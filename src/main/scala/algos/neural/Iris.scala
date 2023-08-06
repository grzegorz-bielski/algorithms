package algos.neural

import scala.util.Random

object Iris:
  @main def run = classify().fold(println, println)

  private lazy val dataSet =
    import scala.language.unsafeNulls

    scala.io.Source
      .fromResource("iris.csv")
      .getLines
      .map(_.split(","))
      .toArray

  private lazy val (
    rawIrisParameters,
    irisClassifications,
    irisSpecies
  ) =
    val init =
      (Array.empty[Array[Double]], Array.empty[Array[Double]], Array.empty[String])

    // shuffle to train on a different subset of data each run
    Random
      .shuffle(dataSet)
      .foldLeft(init):
        case (acc, Array(sl, sw, pl, pw, species)) =>
          val parameters = Array(sl.toDouble, sw.toDouble, pl.toDouble, pw.toDouble)
          val classification = species match
            case "Iris-setosa"     => Array(1d, 0, 0)
            case "Iris-versicolor" => Array(0, 1d, 0)
            case "Iris-virginica"  => Array(0, 0, 1d)

          (acc._1 :+ parameters, acc._2 :+ classification, acc._3 :+ species)

  private lazy val irisParameters = Normalization.byFeatureScaling(rawIrisParameters)

  def interpretOutput(output: Array[Double]): String =
    val maxIndex = output.zipWithIndex.maxBy(_._1)._2

    maxIndex match
      case 0 => "Iris-setosa"
      case 1 => "Iris-versicolor"
      case 2 => "Iris-virginica"

  def classify(): Either[String, Network[String]#ValidationResult] =
    Network
      .create[String](layerStructure = List(4, 6, 3), learningRate = 0.3)(using Random)
      .map: network =>

        // train over the first 140 irises in the data set 50 times
        val parameters = irisParameters.take(140)
        val classifications = irisClassifications.take(140)
        (1 to 50).foreach(_ => network.train(parameters, classifications))

        // test over the last 10 of the irises in the data set
        val testParameters = irisParameters.takeRight(10)
        val expectedSpecies = irisSpecies.takeRight(10)
        network.validate(testParameters, expectedSpecies, interpretOutput)
