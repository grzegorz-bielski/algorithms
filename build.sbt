import Dependencies._

ThisBuild / scalaVersion := "2.13.1"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.example"
ThisBuild / organizationName := "example"

val scalaActic = "org.scalactic" %% "scalactic" % "3.1.0"
val scalaTest = "org.scalatest" %% "scalatest" % "3.1.0"

lazy val root = (project in file("."))
  .settings(
    name := "algorithms",
    libraryDependencies += scalaActic,
    libraryDependencies += scalaTest % Test
  )
