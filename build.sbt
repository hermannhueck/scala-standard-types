lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.8"
lazy val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.14.0"

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "scala-standard-types",
      scalaVersion := "2.13.0",
      version      := "0.1.0-SNAPSHOT",
      scalacOptions := Seq(
      "-unchecked",
      "-deprecation",
      "-feature",
      "-Ywarn-unused",
      "-Ywarn-dead-code",
      "-Ywarn-value-discard"
    )
  )),
  name := "scala-standard-types",
  libraryDependencies ++= Seq(scalaTest % Test, scalaCheck % Test)
)
