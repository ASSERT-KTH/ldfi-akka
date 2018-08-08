name := "ldfi-akka"

version := "1.1.1"

scalaVersion := "2.12.4"

lazy val myproject = project.settings(
  scalaVersion := "2.12.4",
  scalacOptions += "-Ywarn-unused-import"
)

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-testkit" % "2.5.11" % Test,
  "com.typesafe.akka" %% "akka-testkit" % "2.5.11",
  "com.typesafe.akka" %% "akka-actor" % "2.4.20",
  "com.typesafe.akka" %% "akka-slf4j" % "2.5.6",
  "com.typesafe.akka"          %% "akka-persistence" % "2.5.13",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test",
  "org.sat4j" % "org.sat4j.core" % "2.3.1",
  "org.apache.commons" % "commons-io" % "1.3.2",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "ch.epfl.scala" %% "scalafix-core" % "0.6.0-M5",
  "org.scalameta" %% "scalameta" % "3.7.4",
  "org.scalameta" %% "langmeta" % "3.7.4"
)
