lazy val common = Seq(
  organization := "me.romac",
  licenses := Seq(
    "Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")
  ),
  homepage := Some(url("https://github.com/romac/chord")),
  version := Versions.chord,
  scalaVersion := Versions.scala3,
  scalacOptions ++= Seq("-source", "3.3")
)

lazy val root = project
  .in(file("."))
  .settings(common)
  .settings(
    name := "chord",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % Versions.cats,
      "org.typelevel" %% "cats-free" % Versions.cats,
      "org.typelevel" %% "cats-effect" % Versions.catsEffect,
      "org.scalameta" %% "munit" % Versions.munit % Test
    )
  )

lazy val examples = project
  .in(file("examples"))
  .settings(common)
  .settings(
    name := "chord-examples",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % Versions.cats,
      "org.typelevel" %% "cats-effect" % Versions.catsEffect
    )
  )
  .dependsOn(root)
