import sbt.Keys.libraryDependencies

lazy val V = _root_.scalafix.Versions
// Use a scala version supported by scalafix.
scalaVersion in ThisBuild := V.scala212

lazy val rules = project.settings(
  libraryDependencies += "ch.epfl.scala" %% "scalafix-core" % V.version
)

lazy val input = project.settings(
  scalafixSourceroot := sourceDirectory.in(Compile).value,
  libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.4.20"
)

lazy val output = project
    .settings(
      libraryDependencies += "com.typesafe.akka" %% "akka-testkit" % "2.5.11" % Test,
      libraryDependencies += "com.typesafe.akka" %% "akka-testkit" % "2.5.11",
      libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.4.20"
    )

lazy val tests = project
  .settings(
    libraryDependencies += "ch.epfl.scala" % "scalafix-testkit" % V.version % Test cross CrossVersion.full,
    buildInfoPackage := "fix",
    buildInfoKeys := Seq[BuildInfoKey](
      "inputSourceroot" ->
        sourceDirectory.in(input, Compile).value,
      "outputSourceroot" ->
        sourceDirectory.in(output, Compile).value,
      "inputClassdirectory" ->
        classDirectory.in(input, Compile).value
    )
  )
  .dependsOn(input, rules)
  .enablePlugins(BuildInfoPlugin)
