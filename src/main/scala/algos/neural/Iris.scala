package algos.neural

@main def irisData = println(Iris.dataSet)

object Iris:
    lazy val dataSet = scala.io.Source
        .fromResource("iris.csv")
        .getLines
        .map(_.split(","))
        // .collect:
        //     case Array(sl, sw, pl, pw, species) =>
        //         Array(sl.toDouble, sw.toDouble, pl.toDouble, pw.toDouble, species)
        // .map(_.init)
        // .toArray