import sbtassembly.Plugin.AssemblyKeys.{assemblyOption, jarName}
import spray.revolver.RevolverPlugin.Revolver

name := "scaladiagram"

version := "0.1"

//scalaVersion := "2.10.6"

scalaVersion in ThisBuild := "2.11.8"
scalacOptions ++= Seq("-deprecation", "-feature")

// https://mvnrepository.com/artifact/org.scalatest/scalatest
libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % Test

// https://mvnrepository.com/artifact/org.scalariform/scalariform
libraryDependencies += "org.scalariform" %% "scalariform" % "0.2.0"

Revolver.settings

parallelExecution in Test := false

assemblySettings

jarName in assembly := s"scaladiagram-0.1.jar"

test in assembly := {}
