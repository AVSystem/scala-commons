import com.typesafe.sbt.SbtPgp.autoImportImpl.PgpKeys._

cancelable in Global := true

inThisBuild(Seq(
  scalaVersion := "2.11.8",
  organization := "com.avsystem.commons",
  compileOrder := CompileOrder.Mixed,
  scalacOptions ++= Seq(
    "-feature",
    "-deprecation",
    "-unchecked",
    "-language:implicitConversions",
    "-language:existentials",
    "-language:dynamics",
    "-language:experimental.macros",
    "-language:higherKinds",
    "-Xfuture",
    "-Xfatal-warnings",
    "-Xlint:_,-missing-interpolator,-adapted-args"
  )
))

val silencerVersion = "0.3"
val guavaVersion = "18.0"
val jsr305Version = "3.0.0"
val scalatestVersion = "2.2.5"
val upickleVersion = "0.3.6"
val jettyVersion = "8.1.17.v20150415"
val mongoVersion = "3.2.2"
val springVersion = "4.0.2.RELEASE"
val typesafeConfigVersion = "1.3.0"

val commonSettings = Seq(
  sonatypeProfileName := "com.avsystem",
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases" at nexus + "service/local/staging/deploy/maven2")
  },

  projectInfo := ModuleInfo(
    nameFormal = "AVSystem commons",
    description = "AVSystem commons library for Scala",
    homepage = Some(url("https://github.com/AVSystem/scala-commons")),
    startYear = Some(2015),
    organizationName = "AVSystem",
    organizationHomepage = Some(url("http://www.avsystem.com/")),
    scmInfo = Some(ScmInfo(
      browseUrl = url("https://github.com/AVSystem/scala-commons.git"),
      connection = "scm:git:git@github.com:AVSystem/scala-commons.git",
      devConnection = Some("scm:git:git@github.com:AVSystem/scala-commons.git")
    )),
    licenses = Seq(
      ("The MIT License", url("https://opensource.org/licenses/MIT"))
    )
  ),

  publishMavenStyle := true,
  pomIncludeRepository := { _ => false },
  pomExtra := {
    <developers>
      <developer>
        <id>ghik</id>
        <name>Roman Janusz</name>
        <url>https://github.com/ghik</url>
      </developer>
    </developers>
  },

  libraryDependencies += compilerPlugin("com.github.ghik" % "silencer-plugin" % silencerVersion),
  libraryDependencies ++= Seq(
    "com.github.ghik" % "silencer-lib" % silencerVersion,
    "org.scalatest" %% "scalatest" % scalatestVersion % Test
  ),
  dependencyOverrides += "org.scala-lang.modules" %% "scala-xml" % "1.0.4",
  ideBasePackages := Seq(organization.value),
  fork in Test := true
)

val noPublishSettings = Seq(
  publishArtifact := false,
  publish :=(),
  publishLocal :=(),
  publishM2 :=(),
  publishSigned :=(),
  publishLocalSigned :=()
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
    `commons-mongo`,
    `commons-spring`
  )
  .settings(name := "commons")
  .settings(commonSettings: _*)
  .settings(noPublishSettings: _*)

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
  .settings(noPublishSettings: _*)
  .enablePlugins(JmhPlugin)

lazy val `commons-mongo` = project
  .dependsOn(`commons-core`)
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "org.mongodb" % "mongodb-driver" % mongoVersion
    )
  )

lazy val `commons-spring` = project
  .dependsOn(`commons-core`)
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "org.springframework" % "spring-context" % springVersion,
      "com.typesafe" % "config" % typesafeConfigVersion
    )
  )
