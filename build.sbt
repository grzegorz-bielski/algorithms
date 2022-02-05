scalaVersion := "3.1.1"

lazy val root = (project in file("."))
  .settings(
    name := "algorithms",
    libraryDependencies ++= Seq(
      "org.scalactic" %% "scalactic" % "3.2.10",
      "org.scalatest" %% "scalatest" % "3.2.10" % Test
    )
  )

scalacOptions ++= Seq(
  "-new-syntax",
  "-deprecation", 
  "-encoding", "utf-8",
  "-explain-types",
  "-feature",
  "-unchecked",
  "-Xfatal-warnings",
  "-Yexplicit-nulls"      
)