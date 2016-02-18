import sbt._

name := "commons"

version in ThisBuild := "1.11.10"
scalaVersion in ThisBuild := "2.11.7"
organization in ThisBuild := "com.avsystem.commons"
scalacOptions in ThisBuild ++= Seq(
  "-feature",
  "-deprecation",
  "-unchecked",
  "-language:implicitConversions",
  "-language:existentials",
  "-language:dynamics",
  "-language:experimental.macros",
  "-Xfuture",
  "-Xfatal-warnings",
  "-Xlint:_,-missing-interpolator,-adapted-args"
)

externalResolvers in ThisBuild := Seq(
  "AVSystem lib releases" at "http://repo.avsystem.com/libs-releases",
  "AVSystem lib snapshots" at "http://repo.avsystem.com/libs-snapshots",
  "AVSystem remote repository cache" at "http://repo.avsystem.com/remote-repos"
)

publishTo in ThisBuild := {
  val name = if (isSnapshot.value) "snapshots" else "releases"
  Some(name at s"http://repo.avsystem.com/libs-$name-local/")
}
publishMavenStyle in ThisBuild := true

/* The file with credentials must have following format:
 *
 * realm=Artifactory Realm
 * host=repo.avsystem.com
 * user=<LDAP user>
 * password=<LDAP password>
 *
 */
credentials in ThisBuild +=
  Credentials(Path.userHome / ".repo.avsystem.com.credentials")

val silencerVersion = "0.3"
val guavaVersion = "14.0.1"
val jsr305Version = "3.0.0"
val scalatestVersion = "2.2.5"
val upickleVersion = "0.3.6"
val jettyVersion = "8.1.17.v20150415"
val mongoVersion = "3.2.2"
val prevAnalyzerVersion = "1.11.10"

val commonSettings = Seq(
  (publishArtifact in packageDoc) := false,
  libraryDependencies += compilerPlugin("com.github.ghik" % "silencer-plugin" % silencerVersion),
  libraryDependencies += compilerPlugin("com.avsystem.commons" %% "commons-analyzer" % prevAnalyzerVersion),
  libraryDependencies ++= Seq(
    "com.github.ghik" % "silencer-lib" % silencerVersion,
    "org.scalatest" %% "scalatest" % scalatestVersion % Test
  ),
  dependencyOverrides += "org.scala-lang.modules" %% "scala-xml" % "1.0.4",
  ideBasePackages := Seq(organization.value),
  fork in Test := true
)

val CompileAndTest = "compile->compile;test->test"

lazy val commons = project.in(file("."))
  .aggregate(
    `commons-annotations`,
    `commons-macros`,
    `commons-sharedJVM`,
    `commons-sharedJS`,
    `commons-core`,
    `commons-analyzer`,
    `commons-jetty`,
    `commons-benchmark`,
    `commons-mongo`
  )
  .settings(
    publishArtifact := false
  )

lazy val `commons-annotations` = project
  .settings(commonSettings: _*)

lazy val `commons-macros` = project
  .dependsOn(`commons-annotations`)
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
  )

lazy val `commons-shared` = crossProject.crossType(CrossType.Pure)
  .jsConfigure(_.dependsOn(`commons-macros`))
  .jvmConfigure(_.dependsOn(`commons-macros`))
  .settings(commonSettings: _*)
  .jsSettings(
    test := {},
    fork in Test := false
  )

lazy val `commons-sharedJVM` = `commons-shared`.jvm
lazy val `commons-sharedJS` = `commons-shared`.js

lazy val `commons-core` = project.dependsOn(`commons-macros` % CompileAndTest, `commons-sharedJVM`)
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "com.google.code.findbugs" % "jsr305" % jsr305Version,
      "com.google.guava" % "guava" % guavaVersion
    )
  )

lazy val `commons-analyzer` = project
  .dependsOn(`commons-core` % Test)
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value
  )

lazy val `commons-jetty` = project
  .dependsOn(`commons-sharedJVM`)
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "org.eclipse.jetty" % "jetty-client" % jettyVersion,
      "org.eclipse.jetty" % "jetty-server" % jettyVersion,
      "com.lihaoyi" %% "upickle" % upickleVersion % Test
    )
  )

lazy val `commons-benchmark` = project
  .dependsOn(`commons-core`)
  .settings(commonSettings: _*)
  .settings(
    publishArtifact := false
  )
  .enablePlugins(JmhPlugin)

lazy val `commons-mongo` = project
  .dependsOn(`commons-core`)
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "org.mongodb" % "mongodb-driver" % mongoVersion
    )
  )
