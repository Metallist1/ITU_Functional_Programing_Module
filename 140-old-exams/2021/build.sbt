name := "exam"

version := "0.5"

ThisBuild / scalaVersion := "2.13.5"

val scalatestVersion = "3.2.10"

scalacOptions ++= Seq (
  "-Ymacro-annotations",
  "-deprecation", 
  "-feature", 
  "-language:implicitConversions"
)
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.15.4" 
libraryDependencies += "org.scalatest" %% "scalatest-freespec" % scalatestVersion
libraryDependencies += "org.scalatest" %% "scalatest-shouldmatchers" % scalatestVersion
libraryDependencies += "org.scalatestplus" %% "scalacheck-1-15" % (scalatestVersion+".0")

libraryDependencies += "dev.optics" %% "monocle-core"  % "3.0.0"
libraryDependencies += "dev.optics" %% "monocle-macro" % "3.0.0"
libraryDependencies += "org.typelevel" %% "cats-core" % "2.3.0-M2"

// It is easier to read the results if they don't run in parallel.
Test / parallelExecution := false
Test / testOptions += Tests.Argument("-oD")

ThisBuild / console / initialCommands := """
import adpro._
"""
