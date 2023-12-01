ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .settings(
    name := "adventofcode-2023-scala3",
    idePackagePrefix := Some("us.jkk.aoc2023")
  )
