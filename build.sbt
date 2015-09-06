lazy val root = (project in file(".")).
  settings(
    name := "failfast-generator",
    version := "0.1.0",
    scalaVersion := "2.11.7",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
    libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.5",
    doctestSettings
  )
