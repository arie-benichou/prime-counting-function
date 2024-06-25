ThisBuild / scalaVersion := "2.13.14"

lazy val root = (project in file("."))
  .settings(
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.9" % Test,
    libraryDependencies += "com.lihaoyi" %% "upickle" % "3.3.1",
    libraryDependencies += "com.lihaoyi" %% "os-lib" % "0.10.2"
    // libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.21012",
    // libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",
    // libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
  )
