scalaVersion := "2.13.4"

lazy val root = (project in file("."))
  .settings(
    name := "algorithms",
    libraryDependencies ++= Seq(
      "org.scalactic" %% "scalactic" % "3.2.2",
      "org.scalatest" %% "scalatest" % "3.2.2" % Test,
      "org.scalanlp" %% "breeze" % "1.1",
      "org.scalanlp" %% "breeze-natives" % "1.1"
    )
  )
