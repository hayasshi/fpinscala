import sbt._
import sbt.Keys._

object FpinscalaBuild extends Build {

  lazy val fpinscala = Project(
    id = "fpinscala",
    base = file("."),
    settings = Project.defaultSettings ++ Seq(
      name := "fpinscala",
      organization := "fpinscala",
      version := "0.1-SNAPSHOT",
      scalaVersion := "2.11.7"
      // add other settings here
    )
  )
}
