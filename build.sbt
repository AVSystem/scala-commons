import com.typesafe.sbt.SbtPgp.autoImportImpl.PgpKeys._

cancelable in Global := true

inThisBuild(Seq(
  scalaVersion := "2.12.3",
  crossScalaVersions := Seq("2.11.11", "2.12.3"),
  organization := "com.avsystem.commons",
  compileOrder := CompileOrder.Mixed,
  scalacOptions ++= Seq(
    "-encoding", "utf-8",
    "-explaintypes",
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
    "-Xlint:-missing-interpolator,-adapted-args,-unused,_",
  ),
  scalacOptions := {
    val opts = scalacOptions.value
    if (scalaBinaryVersion.value == "2.11")
      opts.map(o => if (o.startsWith("-Xlint")) o.replace("-unused,", "") else o)
    else
      opts
  },
  apiURL := Some(url("http://avsystem.github.io/scala-commons/api")),
  autoAPIMappings := true,
))

// for binary compatibility checking
val previousVersion = "1.22.0"

val silencerVersion = "0.5"
val guavaVersion = "23.0"
val jsr305Version = "3.0.2"
val scalatestVersion = "3.0.4"
val upickleVersion = "0.4.4"
val scalacheckVersion = "1.13.5"
val jettyVersion = "9.3.8.v20160314"
val mongoVersion = "3.5.0"
val kafkaVersion = "0.11.0.1"
val mongoScalaVersion = "2.1.0"
val springVersion = "4.0.2.RELEASE"
val typesafeConfigVersion = "1.3.1"
val commonsIoVersion = "1.3.2"
val scalaLoggingVersion = "3.5.0"
val akkaVersion = "2.5.6"
val monixVersion = "2.3.0"
val mockitoVersion = "2.10.0"
val circeVersion = "0.8.0"

val commonSettings = Seq(
  publishTo := Some(Opts.resolver.sonatypeStaging),
  sonatypeProfileName := "com.avsystem",

  projectInfo := ModuleInfo(
    nameFormal = "AVSystem commons",
    description = "AVSystem commons library for Scala",
    homepage = Some(url("https://github.com/AVSystem/scala-commons")),
    startYear = Some(2015),
    licenses = Vector(
      "The MIT License" -> url("https://opensource.org/licenses/MIT"),
    ),
    organizationName = "AVSystem",
    organizationHomepage = Some(url("http://www.avsystem.com/")),
    scmInfo = Some(ScmInfo(
      browseUrl = url("https://github.com/AVSystem/scala-commons.git"),
      connection = "scm:git:git@github.com:AVSystem/scala-commons.git",
      devConnection = Some("scm:git:git@github.com:AVSystem/scala-commons.git"),
    )),
    developers = Vector(
      Developer("ghik", "Roman Janusz", "r.janusz@avsystem.com", url("https://github.com/ghik")),
    ),
  ),

  publishMavenStyle := true,
  pomIncludeRepository := { _ => false },

  libraryDependencies += compilerPlugin("com.github.ghik" %% "silencer-plugin" % silencerVersion),
  libraryDependencies ++= Seq(
    "com.github.ghik" %% "silencer-lib" % silencerVersion,
    "org.scalatest" %% "scalatest" % scalatestVersion % Test,
    "org.scalacheck" %% "scalacheck" % scalacheckVersion % Test,
    "org.apache.commons" % "commons-io" % commonsIoVersion % Test,
  ),
  dependencyOverrides += "org.scala-lang.modules" %% "scala-xml" % "1.0.6",
  ideBasePackages := Seq(organization.value),
  ideOutputDirectory in Compile := Some(baseDirectory.value / "out/production"),
  ideOutputDirectory in Test := Some(baseDirectory.value / "out/test"),
  fork in Test := true,
)

val jvmCommonSettings = Seq(
  mimaPreviousArtifacts := {
    Set(organization.value % s"${name.value}_${scalaBinaryVersion.value}" % previousVersion)
  },
)

val noPublishSettings = Seq(
  publishArtifact := false,
  publish := {},
  publishLocal := {},
  publishM2 := {},
  publishSigned := {},
  publishLocalSigned := {},
  doc := (target in doc).value,
  mimaPreviousArtifacts := Set.empty,
)

val CompileAndTest = "compile->compile;test->test"

lazy val commons = project.in(file("."))
  .enablePlugins(ScalaUnidocPlugin)
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
    `commons-akka`,
    `commons-kafka`,
  )
  .settings(commonSettings: _*)
  .settings(noPublishSettings: _*)
  .settings(
    name := "commons",
    scalacOptions in(ScalaUnidoc, unidoc) += "-Ymacro-no-expand",
    unidocProjectFilter in(ScalaUnidoc, unidoc) :=
      inAnyProject -- inProjects(
        `commons-macros`,
        `commons-analyzer`,
        `commons-sharedJS`,
        `commons-benchmark`,
      ),
  )

lazy val `commons-annotations` = project
  .settings(commonSettings: _*)
  .settings(jvmCommonSettings: _*)

lazy val `commons-macros` = project
  .dependsOn(`commons-annotations`)
  .settings(commonSettings: _*)
  .settings(jvmCommonSettings: _*)
  .settings(
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  )

lazy val `commons-shared` = crossProject.crossType(CrossType.Pure)
  .jsConfigure(_.dependsOn(`commons-macros`))
  .jvmConfigure(_.dependsOn(`commons-macros`))
  .settings(commonSettings: _*)
  .jvmSettings(jvmCommonSettings)
  .jsSettings(
    scalaJSUseMainModuleInitializer in Test := true,
    scalacOptions += {
      val localDir = (baseDirectory in ThisBuild).value.toURI.toString
      val githubDir = "https://raw.githubusercontent.com/AVSystem/scala-commons"
      s"-P:scalajs:mapSourceURI:$localDir->$githubDir/v${version.value}/"
    },
    test := {},
    fork in Test := false,
  )

lazy val `commons-sharedJVM` = `commons-shared`.jvm
lazy val `commons-sharedJS` = `commons-shared`.js

lazy val `commons-core` = project.dependsOn(`commons-macros` % CompileAndTest, `commons-sharedJVM`)
  .settings(commonSettings: _*)
  .settings(jvmCommonSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "com.google.code.findbugs" % "jsr305" % jsr305Version,
      "com.google.guava" % "guava" % guavaVersion,
    ),
  )

lazy val `commons-analyzer` = project
  .dependsOn(`commons-core` % Test)
  .settings(commonSettings: _*)
  .settings(jvmCommonSettings: _*)
  .settings(
    libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value,
  )

lazy val `commons-jetty` = project
  .dependsOn(`commons-sharedJVM`)
  .settings(commonSettings: _*)
  .settings(jvmCommonSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "org.eclipse.jetty" % "jetty-client" % jettyVersion,
      "org.eclipse.jetty" % "jetty-server" % jettyVersion,
      "com.lihaoyi" %% "upickle" % upickleVersion,
    ),
  )

lazy val `commons-benchmark` = project
  .dependsOn(`commons-core`, `commons-akka`, `commons-redis`, `commons-mongo`)
  .settings(commonSettings: _*)
  .settings(jvmCommonSettings: _*)
  .settings(noPublishSettings: _*)
  .enablePlugins(JmhPlugin)
  .settings(
    libraryDependencies ++= {
      if (scalaBinaryVersion.value != "2.12") Seq(
        "com.github.etaty" %% "rediscala" % "1.6.0",
        "com.livestream" %% "scredis" % "2.0.8",
      )
      else Seq.empty
    },
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % circeVersion,
      "io.circe" %% "circe-generic" % circeVersion,
      "io.circe" %% "circe-jawn" % circeVersion,
      "io.circe" %% "circe-parser" % circeVersion,
      "io.circe" %% "circe-testing" % circeVersion % Test,
    ),
    ideExcludedDirectories := (managedSourceDirectories in Jmh).value,
  )

lazy val `commons-mongo` = project
  .dependsOn(`commons-core`)
  .settings(commonSettings: _*)
  .settings(jvmCommonSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "org.mongodb" % "mongodb-driver-core" % mongoVersion,
      "org.mongodb" % "mongodb-driver" % mongoVersion % Optional,
      "org.mongodb" % "mongodb-driver-async" % mongoVersion % Optional,
      "org.mongodb.scala" %% "mongo-scala-driver" % mongoScalaVersion % Optional,
    ),
  )

lazy val `commons-kafka` = project
  .dependsOn(`commons-core`)
  .settings(commonSettings: _*)
  .settings(jvmCommonSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "org.apache.kafka" % "kafka-streams" % kafkaVersion,
    ),
  )

lazy val `commons-redis` = project
  .dependsOn(`commons-core`)
  .settings(commonSettings: _*)
  .settings(jvmCommonSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-actor" % akkaVersion,
      "com.typesafe.scala-logging" %% "scala-logging" % scalaLoggingVersion,
    ),
    parallelExecution in Test := false,
  )

lazy val `commons-spring` = project
  .dependsOn(`commons-core`)
  .settings(commonSettings: _*)
  .settings(jvmCommonSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "org.springframework" % "spring-context" % springVersion,
      "com.typesafe" % "config" % typesafeConfigVersion,
    ),
  )

lazy val `commons-akka` = project
  .dependsOn(`commons-core`)
  .settings(commonSettings: _*)
  .settings(jvmCommonSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-actor" % akkaVersion,
      "com.typesafe.akka" %% "akka-remote" % akkaVersion,
      "io.monix" %% "monix" % monixVersion,
      "com.typesafe.akka" %% "akka-testkit" % akkaVersion % Test,
      "org.mockito" % "mockito-core" % mockitoVersion % Test,
    ),
  )
