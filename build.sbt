
organization := "ch.epfl.systemf"

lazy val gradient = project.in(file("."))
  .settings(
    scalaVersion := "3.5.1",
    version := "0.1.0-SNAPSHOT",
    libraryDependencies ++= Seq(
      "com.novocode" % "junit-interface" % "0.11" % Test
    )
  )
