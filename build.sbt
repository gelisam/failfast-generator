lazy val root = (project in file(".")).
  settings(
    name := "failfast-generator",
    version := "0.1.0",
    doctestSettings
  )
