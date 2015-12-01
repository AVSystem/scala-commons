logLevel := Level.Warn

resolvers += Resolver.url("jetbrains-bintray",
  url("http://dl.bintray.com/jetbrains/sbt-plugins/"))(Resolver.ivyStylePatterns)

addSbtPlugin("org.jetbrains" % "sbt-ide-settings" % "0.1.1")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.5")
