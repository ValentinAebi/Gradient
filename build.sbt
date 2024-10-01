
val scala351 = "3.5.1"
val version010snap = "0.1.0-SNAPSHOT"

organization := "ch.epfl.systemf"

lazy val commons = project.in(file("commons"))
  .settings(
    scalaVersion := scala351,
    version := version010snap
  )

lazy val gradcc = project.in(file("gradcc"))
  .settings(
    scalaVersion := scala351,
    version := version010snap,
    libraryDependencies += "org.junit.jupiter" % "junit-jupiter-api" % "5.11.1" % Test
  ).dependsOn(commons)
