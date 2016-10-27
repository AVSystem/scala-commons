import com.typesafe.sbt.SbtPgp.autoImportImpl.PgpKeys._
import sbtunidoc.Plugin.UnidocKeys._

cancelable in Global := true

inThisBuild(Seq(
  scalaVersion := "2.11.8",
  crossScalaVersions := Seq("2.11.8"),
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
  ),
  apiURL := Some(url("http://avsystem.github.io/scala-commons/api")),
  autoAPIMappings := true
))

val silencerVersion = "0.5"
val guavaVersion = "18.0"
val jsr305Version = "3.0.0"
val scalatestVersion = "3.0.0"
val scalacheckVersion = "1.13.3"
val upickleVersion = "0.3.6"
val jettyVersion = "9.3.8.v20160314"
val mongoVersion = "3.2.2"
val springVersion = "4.0.2.RELEASE"
val typesafeConfigVersion = "1.3.0"
val akkaVersion = "2.4.11"
val commonsIoVersion = "1.3.2"
val scalaLoggingVersion = "3.5.0"
val monixVersion = "2.0.5"

val commonSettings = Seq(
  sonatypeProfileName := "com.avsystem",

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

  libraryDependencies += compilerPlugin("com.github.ghik" %% "silencer-plugin" % silencerVersion),
  libraryDependencies ++= Seq(
    "com.github.ghik" %% "silencer-lib" % silencerVersion,
    "org.scalatest" %% "scalatest" % scalatestVersion % Test,
    "org.scalacheck" %% "scalacheck" % scalacheckVersion % Test,
    "org.apache.commons" % "commons-io" % commonsIoVersion % Test
  ),
  dependencyOverrides += "org.scala-lang.modules" %% "scala-xml" % "1.0.5",
  ideBasePackages := Seq(organization.value),
  ideOutputDirectory in Compile := Some(baseDirectory.value / "out/production"),
  ideOutputDirectory in Test := Some(baseDirectory.value / "out/test"),
  fork in Test := true
)

val noPublishSettings = Seq(
  publishArtifact := false,
  publish := (),
  publishLocal := (),
  publishM2 := (),
  publishSigned := (),
  publishLocalSigned := (),
  doc := (target in doc).value
)

val scala212Settings = Seq(
  crossScalaVersions += "2.12.0-RC2"
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
    `commons-spring`,
    `commons-redis`,
    `commons-akka`
  )
  .settings(commonSettings: _*)
  .settings(unidocSettings: _*)
  .settings(noPublishSettings: _*)
  .settings(
    name := "commons",
    scalacOptions in(ScalaUnidoc, unidoc) += "-Ymacro-no-expand",
    unidocProjectFilter in(ScalaUnidoc, unidoc) :=
      inAnyProject -- inProjects(
        `commons-macros`,
        `commons-analyzer`,
        `commons-sharedJS`,
        `commons-jetty`, // because no upickle for Scala 2.12
        `commons-benchmark`
      )
  )

lazy val `commons-annotations` = project
  .settings(commonSettings: _*)
  .settings(scala212Settings: _*)

lazy val `commons-macros` = project
  .dependsOn(`commons-annotations`)
  .settings(commonSettings: _*)
  .settings(scala212Settings: _*)
  .settings(
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
  )

lazy val `commons-shared` = crossProject.crossType(CrossType.Pure)
  .jsConfigure(_.dependsOn(`commons-macros`))
  .jvmConfigure(_.dependsOn(`commons-macros`))
  .settings(commonSettings: _*)
  .settings(scala212Settings: _*)
  .jsSettings(
    scalacOptions += {
      val localDir = (baseDirectory in ThisBuild).value.toURI.toString
      val githubDir = "https://raw.githubusercontent.com/AVSystem/scala-commons"
      s"-P:scalajs:mapSourceURI:$localDir->$githubDir/v${version.value}/"
    },
    test := {},
    fork in Test := false
  )

lazy val `commons-sharedJVM` = `commons-shared`.jvm
lazy val `commons-sharedJS` = `commons-shared`.js

lazy val `commons-core` = project.dependsOn(`commons-macros` % CompileAndTest, `commons-sharedJVM`)
  .settings(commonSettings: _*)
  .settings(scala212Settings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "com.google.code.findbugs" % "jsr305" % jsr305Version,
      "com.google.guava" % "guava" % guavaVersion
    )
  )

lazy val `commons-analyzer` = project
  .dependsOn(`commons-core` % Test)
  .settings(commonSettings: _*)
  .settings(scala212Settings: _*)
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
  .dependsOn(`commons-core`, `commons-akka`, `commons-redis`)
  .settings(commonSettings: _*)
  .settings(noPublishSettings: _*)
  .enablePlugins(JmhPlugin)
  .settings(
    ideExcludedDirectories := (managedSourceDirectories in Jmh).value
  )

lazy val `commons-mongo` = project
  .dependsOn(`commons-core`)
  .settings(scala212Settings: _*)
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "org.mongodb" % "mongodb-driver" % mongoVersion
    )
  )

lazy val `commons-redis` = project
  .dependsOn(`commons-core`)
  .settings(scala212Settings: _*)
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-actor" % akkaVersion,
      "com.typesafe.scala-logging" %% "scala-logging" % scalaLoggingVersion
    ),
    parallelExecution in Test := false
  )

lazy val `commons-spring` = project
  .dependsOn(`commons-core`)
  .settings(scala212Settings: _*)
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "org.springframework" % "spring-context" % springVersion,
      "com.typesafe" % "config" % typesafeConfigVersion
    )
  )

lazy val `commons-akka` = project
  .dependsOn(`commons-core`)
  .settings(scala212Settings: _*)
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-actor" % akkaVersion,
      "com.typesafe.akka" %% "akka-remote" % akkaVersion,
      "io.monix" %% "monix" % monixVersion,
      "com.typesafe.akka" %% "akka-testkit" % akkaVersion % Test,
      "org.mockito" % "mockito-core" % "2.0.54-beta" % Test
    )
  )
