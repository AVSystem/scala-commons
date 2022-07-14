import sbt.Keys._
import sbt._

object BuildBuild extends AutoPlugin {
  val scalaJSVersion: String =
    Option(System.getenv("SCALAJS_VERSION")).getOrElse("1.10.1")

  override def extraProjects: Seq[Project] = Seq(macros)

  lazy val root = project.in(file("."))
    .enablePlugins(this)
    .dependsOn(macros)
    .settings(
      logLevel := Level.Warn,

      addSbtPlugin("org.scala-js" % "sbt-scalajs" % scalaJSVersion),
      addSbtPlugin("org.jetbrains.scala" % "sbt-ide-settings" % "1.1.1"),
      addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.0"),
      addSbtPlugin("com.geirsson" % "sbt-ci-release" % "1.5.7"),
      addSbtPlugin("com.eed3si9n" % "sbt-unidoc" % "0.4.3"),
      addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "0.9.2"),
      addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.6.2"),
      addSbtPlugin("ch.epfl.scala" % "sbt-bloop" % "1.4.8"),
      addSbtPlugin("com.codecommit" % "sbt-github-actions" % "0.9.5"),
    )

  lazy val macros = project
    .settings(
      libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
    )
}
