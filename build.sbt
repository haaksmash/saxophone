name := """saxophone"""

version := "1.1.1"

lazy val root = (project in file("."))

scalaVersion := "2.11.4"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"

mainClass in (Compile, run) := Some("com.haaksmash.saxophone.Application")
