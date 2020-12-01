scalaVersion := "2.13.4"

val scalaActic = "org.scalactic" %% "scalactic" % "3.2.2"
val scalaTest = "org.scalatest" %% "scalatest" % "3.2.2"

lazy val root = (project in file("."))
  .settings(
    name := "algorithms",
    libraryDependencies += scalaActic,
    libraryDependencies += scalaTest % Test
  )
