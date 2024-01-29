ThisBuild / organization := "me.romac"
ThisBuild / homepage := Some(url("https://github.com/romac/chord"))
ThisBuild / licenses := Seq(
  "Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")
)

ThisBuild / version := Versions.chord
ThisBuild / scalaVersion := Versions.scala3
ThisBuild / scalacOptions ++= Seq("-source", "3.3")

lazy val root = project
  .in(file("."))
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
  .settings(
    name := "chord-examples",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % Versions.cats,
      "org.typelevel" %% "cats-effect" % Versions.catsEffect
    )
  )
  .dependsOn(root)

val PrimaryJava = JavaSpec.temurin("8")
val LTSJava = JavaSpec.temurin("17")
val GraalVM = JavaSpec.graalvm(Graalvm.Distribution("graalvm-community"), "17")

ThisBuild / githubWorkflowJavaVersions := Seq(PrimaryJava, LTSJava, GraalVM)

ThisBuild / githubWorkflowTargetTags ++= Seq("v*")

ThisBuild / githubWorkflowPublishTargetBranches :=
  Seq(RefPredicate.StartsWith(Ref.Tag("v")))

// ThisBuild / githubWorkflowPublish := Seq(
//   WorkflowStep.Sbt(
//     commands = List("ci-release"),
//     name = Some("Publish project")
//   )
// )
//
// ThisBuild / githubWorkflowPublishTargetBranches :=
//   Seq(
//     RefPredicate.StartsWith(Ref.Tag("v")),
//     RefPredicate.Equals(Ref.Branch("main"))
//   )
//
// ThisBuild / githubWorkflowPublish := Seq(
//   WorkflowStep.Sbt(
//     commands = List("ci-release"),
//     name = Some("Publish project"),
//     env = Map(
//       "PGP_PASSPHRASE" -> "${{ secrets.PGP_PASSPHRASE }}",
//       "PGP_SECRET" -> "${{ secrets.PGP_SECRET }}",
//       "SONATYPE_PASSWORD" -> "${{ secrets.SONATYPE_PASSWORD }}",
//       "SONATYPE_USERNAME" -> "${{ secrets.SONATYPE_USERNAME }}"
//     )
//   )
// )
