logLevel := Level.Warn

resolvers += Resolver.url("jetbrains-bintray",
  url("https://dl.bintray.com/jetbrains/sbt-plugins/"))(Resolver.ivyStylePatterns)

val scalaJSVersion =
  Option(System.getenv("SCALAJS_VERSION")).getOrElse("1.0.1")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % scalaJSVersion)
addSbtPlugin("org.jetbrains" % "sbt-ide-settings" % "1.0.0")
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.3.7")
addSbtPlugin("com.jsuereth" % "sbt-pgp" % "2.0.0")
addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "3.8.1")
addSbtPlugin("com.eed3si9n" % "sbt-unidoc" % "0.4.2")
addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "0.6.1")
addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.5.0")
addSbtPlugin("ch.epfl.scala" % "sbt-bloop" % "1.3.5")
