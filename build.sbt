name := """hst"""

version := "0.1.0"

lazy val root = (project in file("."))

scalaVersion := "2.11.1"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3"
