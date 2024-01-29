lazy val root = project
  .in(file("."))
  .settings(
    name := "chord",
    version := Versions.chord,
    scalaVersion := Versions.scala3,
    scalacOptions ++= Seq("-source", "3.3"),
    libraryDependencies += "org.typelevel" %% "cats-core" % Versions.cats,
    libraryDependencies += "org.typelevel" %% "cats-kernel" % Versions.cats,
    libraryDependencies += "org.typelevel" %% "cats-free" % Versions.cats,
    libraryDependencies += "org.typelevel" %% "cats-effect" % Versions.catsEffect,
    libraryDependencies += "org.scalameta" %% "munit" % Versions.munit % Test
  )

lazy val examples = project
  .in(file("examples"))
  .settings(
    name := "chord-examples",
    version := Versions.chord,
    scalaVersion := Versions.scala3,
    scalacOptions ++= Seq("-source", "3.3"),
    libraryDependencies += "org.typelevel" %% "cats-core" % Versions.cats,
    libraryDependencies += "org.typelevel" %% "cats-kernel" % Versions.cats,
    libraryDependencies += "org.typelevel" %% "cats-effect" % Versions.catsEffect
  )
  .dependsOn(root)
