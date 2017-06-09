name := """scala-99"""

version := "1.0"

crossScalaVersions := Seq("2.12.0", "2.11.7", "2.10.5", "2.9.3")

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.6" % "test"
)
