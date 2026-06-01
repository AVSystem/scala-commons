logLevel := Level.Warn

addSbtPlugin("com.github.ghik" % "sbt-nosbt" % "0.2.1")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.21.0")
addSbtPlugin("org.scala-js" % "sbt-jsdependencies" % "1.0.2")
addSbtPlugin("org.jetbrains.scala" % "sbt-ide-settings" % "1.1.4")
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.8")
// TODO[scala3-port]: re-enable sbt-ci-release — transitively pulls sbt-git whose JGit chokes on linked worktrees (NoWorkTreeException). Disabled to unblock per-branch builds in worktrees.
//addSbtPlugin("com.github.sbt" % "sbt-ci-release" % "1.11.2")
addSbtPlugin("com.github.sbt" % "sbt-unidoc" % "0.6.1")
addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.6.4")
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.6.1")
addSbtPlugin("com.github.sbt" % "sbt-github-actions" % "0.30.0")
addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "1.1.4")
