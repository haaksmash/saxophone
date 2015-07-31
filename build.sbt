name := """saxophone"""

version := "2.0.0"

organization := "com.haaksmash"

lazy val root = (project in file("."))

scalaVersion := "2.11.6"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"

mainClass in (Compile, run) := Some("com.haaksmash.saxophone.Application")

publishMavenStyle := true

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

licenses := Seq("GPL v3" -> url("http://www.gnu.org/licenses/gpl-3.0.en.html"))

homepage := Some(url("http://github.com/haaksmash/saxophone"))

scmInfo := Some(ScmInfo(url("http://github.com/haaksmash/saxophone"), "scm:git@github.com:haaksmash/saxophone.git"))

pomIncludeRepository := { x => false }

pomExtra := (
  <developers>
    <developer>
      <id>haaksmash</id>
      <name>Haak Saxberg</name>
      <url>http://haaksmash.com</url>
    </developer>
  </developers>)
