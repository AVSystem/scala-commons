lazy val macros = project.settings(
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
)

lazy val root = project.in(file(".")).dependsOn(macros)
