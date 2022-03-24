logLevel := Level.Warn

val scalaJSVersion =
  Option(System.getenv("SCALAJS_VERSION")).getOrElse("1.7.1")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % scalaJSVersion)
addSbtPlugin("org.jetbrains.scala" % "sbt-ide-settings" % "1.1.1")
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.0")
addSbtPlugin("com.geirsson" % "sbt-ci-release" % "1.5.7")
addSbtPlugin("com.eed3si9n" % "sbt-unidoc" % "0.4.3")
addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "0.9.2")
addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.6.0")
addSbtPlugin("ch.epfl.scala" % "sbt-bloop" % "1.4.8")
addSbtPlugin("com.codecommit" % "sbt-github-actions" % "0.9.5")
