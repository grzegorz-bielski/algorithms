scalaVersion := "3.3.0"

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val root = (project in file("."))
  .settings(
    name := "algorithms",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "pprint" % "0.7.0",
      "org.scalactic" %% "scalactic" % "3.2.15",
      "org.scalatest" %% "scalatest" % "3.2.15" % Test
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
  "-Yexplicit-nulls",
  "-Ysafe-init",
  "-Wvalue-discard",
  "-Wunused:all",
  // "-language:strictEquality"
)