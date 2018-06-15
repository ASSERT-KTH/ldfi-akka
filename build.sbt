
name := "ldfi-akka"

version := "1.1.1"

scalaVersion := "2.12.4"


libraryDependencies += "com.typesafe.akka" %% "akka-testkit" % "2.5.11" % Test

libraryDependencies += "com.typesafe.akka" %% "akka-testkit" % "2.5.11"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.4.20"

libraryDependencies += "org.sat4j" % "org.sat4j.core" % "2.3.1"

libraryDependencies += "org.apache.commons" % "commons-io" % "1.3.2"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"

libraryDependencies += "com.typesafe.akka" %% "akka-slf4j" % "2.5.6"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

libraryDependencies += "ch.epfl.scala" %% "scalafix-core" % "0.6.0-M5"

libraryDependencies += "org.scalameta" %% "scalameta" % "3.7.4"

libraryDependencies += "org.typelevel" %% "cats-core" % "1.1.0"

libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.0"

libraryDependencies += "io.monix" %% "monix" % "3.0.0-8084549"

libraryDependencies += "org.scalameta" %% "langmeta" % "3.7.4"

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.5.1")

addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.5.10")

