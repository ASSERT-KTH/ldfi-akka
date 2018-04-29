
name := "ldfi-akka"

version := "1.1.1"

scalaVersion := "2.12.4"

libraryDependencies += "com.typesafe.akka" %% "akka-testkit" % "2.5.11" % Test

libraryDependencies += "com.typesafe.akka" %% "akka-testkit" % "2.5.11"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.4.20"

libraryDependencies += "org.sat4j" % "org.sat4j.sat" % "2.3.0"

libraryDependencies += "org.sat4j" % "org.sat4j.pom" % "2.3.1" pomOnly()

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"

libraryDependencies += "com.typesafe.akka" %% "akka-slf4j" % "2.5.6"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.5.1")

addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.5.10")