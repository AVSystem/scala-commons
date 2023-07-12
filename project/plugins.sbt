val scalaJSVersion: String =
  Option(System.getenv("SCALAJS_VERSION")).getOrElse("1.10.1")

logLevel := Level.Warn

addSbtPlugin("com.github.ghik" % "sbt-nosbt" % "0.2.1")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % scalaJSVersion)
addSbtPlugin("org.jetbrains.scala" % "sbt-ide-settings" % "1.1.1")
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.0")
addSbtPlugin("com.github.sbt" % "sbt-ci-release" % "1.5.11")
addSbtPlugin("com.eed3si9n" % "sbt-unidoc" % "0.4.3")
addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "0.9.2")
addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.6.4")
addSbtPlugin("ch.epfl.scala" % "sbt-bloop" % "1.4.8")
addSbtPlugin("com.codecommit" % "sbt-github-actions" % "0.14.2")
