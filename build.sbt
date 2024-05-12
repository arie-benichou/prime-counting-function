ThisBuild / scalaVersion := "2.13.14"

lazy val root = (project in file("."))
  .settings(
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
    libraryDependencies += "com.lihaoyi" %% "upickle" % "3.1.0",
    libraryDependencies += "com.lihaoyi" %% "os-lib" % "0.9.1",
    libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.10",
    libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5"
    // libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
  )
