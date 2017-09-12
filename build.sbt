name := "predicate"
organization := "org.alleninstitute"

version := "1.1-SNAPSHOT"

scalaVersion := "2.12.0"

resolvers += Resolver.sonatypeRepo("releases")
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"
libraryDependencies += "org.scalactic"  %% "scalactic"  % "3.0.0"
libraryDependencies += "org.scalatest"  %% "scalatest"  % "3.0.0"  % "test"
libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
