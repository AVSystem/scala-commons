import com.typesafe.sbt.SbtPgp.autoImportImpl.PgpKeys._

cancelable in Global := true

// We need to generate slightly different structure for IntelliJ in order to better support ScalaJS cross projects.
// idea.managed property is set by IntelliJ when running SBT (shell or import), idea.runid is set only for IntelliJ's
// SBT shell. In order for this technique to work, you MUST NOT set the "Use the sbt shell for build and import"
// option in IntelliJ's SBT settings.
val forIdeaImport = System.getProperty("idea.managed", "false").toBoolean && System.getProperty("idea.runid") == null

// for binary compatibility checking
val previousVersion = "1.25.0"

val silencerVersion = "0.6"
val guavaVersion = "23.0"
val jsr305Version = "3.0.2"
val scalatestVersion = "3.0.5"
val scalacheckVersion = "1.13.5"
val jettyVersion = "9.3.23.v20180228"
val mongoVersion = "3.5.0"
val kafkaVersion = "0.11.0.1"
val mongoScalaVersion = "2.1.0"
val springVersion = "4.0.2.RELEASE"
val typesafeConfigVersion = "1.3.3"
val commonsIoVersion = "1.3.2"
val scalaLoggingVersion = "3.8.0"
val akkaVersion = "2.5.11"
val monixVersion = "2.3.3"
val mockitoVersion = "2.16.0"
val circeVersion = "0.9.2"

val commonSettings = Seq(
  organization := "com.avsystem.commons",
  scalaVersion := "2.12.4",
  crossScalaVersions := Seq("2.11.11", "2.12.4"),
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
    s"-Xlint:-missing-interpolator,-adapted-args,${if (scalaBinaryVersion.value == "2.12") "-unused," else ""}_",
  ),
  apiURL := Some(url("http://avsystem.github.io/scala-commons/api")),
  autoAPIMappings := true,

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

  libraryDependencies ++= Seq(
    compilerPlugin("com.github.ghik" %% "silencer-plugin" % silencerVersion),
    "com.github.ghik" %% "silencer-lib" % silencerVersion % Optional,
    "org.scalatest" %%% "scalatest" % scalatestVersion % Test,
    "org.scalacheck" %%% "scalacheck" % scalacheckVersion % Test,
  ),
  dependencyOverrides += "org.scala-lang.modules" %% "scala-xml" % "1.0.6",
  ideBasePackages := Seq(organization.value),
  ideOutputDirectory in Compile := Some(target.value.getParentFile / "out/production"),
  ideOutputDirectory in Test := Some(target.value.getParentFile / "out/test"),
  fork in Test := true,
)

val jvmCommonSettings = commonSettings ++ Seq(
  libraryDependencies ++= Seq(
    "org.apache.commons" % "commons-io" % commonsIoVersion % Test,
  ),
  mimaPreviousArtifacts := {
    Set(organization.value % s"${name.value}_${scalaBinaryVersion.value}" % previousVersion)
  },
)

val jsCommonSettings = commonSettings ++ Seq(
  scalacOptions += {
    val localDir = (baseDirectory in ThisBuild).value.toURI.toString
    val githubDir = "https://raw.githubusercontent.com/AVSystem/scala-commons"
    s"-P:scalajs:mapSourceURI:$localDir->$githubDir/v${version.value}/"
  },
  jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv(),
  fork in Test := false,
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
    `commons-core`,
    `commons-core-js`,
    `commons-analyzer`,
    `commons-jetty`,
    `commons-benchmark`,
    `commons-mongo`,
    `commons-spring`,
    `commons-redis`,
    `commons-akka`,
    `commons-kafka`,
  )
  .settings(
    commonSettings,
    noPublishSettings,
    name := "commons",
    scalacOptions in ScalaUnidoc in unidoc += "-Ymacro-expand:none",
    unidocProjectFilter in ScalaUnidoc in unidoc :=
      inAnyProject -- inProjects(
        `commons-macros`,
        `commons-analyzer`,
        `commons-core-js`,
        `commons-benchmark`,
      ),
  )

lazy val `commons-annotations` = project
  .settings(jvmCommonSettings)

lazy val `commons-macros` = project
  .dependsOn(`commons-annotations`)
  .settings(
    jvmCommonSettings,
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  )

def mkSourceDirs(base: File, scalaBinary: String, conf: String): Seq[File] = Seq(
  base / "src" / conf / "scala",
  base / "src" / conf / s"scala-$scalaBinary",
  base / "src" / conf / "java"
)

def sourceDirsSettings(baseMapper: File => File) = Seq(
  unmanagedSourceDirectories in Compile ++=
    mkSourceDirs(baseMapper(baseDirectory.value), scalaBinaryVersion.value, "main"),
  unmanagedSourceDirectories in Test ++=
    mkSourceDirs(baseMapper(baseDirectory.value), scalaBinaryVersion.value, "test"),
)

lazy val `commons-core` = project
  .dependsOn(`commons-macros`)
  .settings(
    jvmCommonSettings,
    sourceDirsSettings(_ / "jvm"),
    libraryDependencies ++= Seq(
      "com.google.code.findbugs" % "jsr305" % jsr305Version % Optional,
      "com.google.guava" % "guava" % guavaVersion % Optional,
    ),
    ideExcludedDirectories := Seq(baseDirectory.value / "agg"),
  )

lazy val `commons-core-js` = project.in(`commons-core`.base / "js")
  .enablePlugins(ScalaJSPlugin)
  .configure(p => if (forIdeaImport) p.dependsOn(`commons-core`) else p)
  .dependsOn(`commons-macros`)
  .settings(
    jsCommonSettings,
    name := (name in `commons-core`).value,
    sourceDirsSettings(_.getParentFile),
  )

lazy val `commons-core-agg` = project.in(`commons-core`.base / "agg")
  .aggregate(`commons-core`, `commons-core-js`)
  .settings(
    commonSettings,
    noPublishSettings,
    ideSkipProject := true
  )

lazy val `commons-analyzer` = project
  .dependsOn(`commons-core` % Test)
  .settings(
    jvmCommonSettings,
    libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value,
  )

lazy val `commons-jetty` = project
  .dependsOn(`commons-core`)
  .settings(
    jvmCommonSettings,
    libraryDependencies ++= Seq(
      "org.eclipse.jetty" % "jetty-client" % jettyVersion,
      "org.eclipse.jetty" % "jetty-server" % jettyVersion,
    ),
  )

lazy val `commons-benchmark` = project
  .dependsOn(`commons-core`, `commons-akka`, `commons-redis`, `commons-mongo`)
  .enablePlugins(JmhPlugin)
  .settings(
    jvmCommonSettings,
    noPublishSettings,
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
  .settings(
    jvmCommonSettings,
    libraryDependencies ++= Seq(
      "com.google.guava" % "guava" % guavaVersion,
      "org.mongodb" % "mongodb-driver-core" % mongoVersion,
      "org.mongodb" % "mongodb-driver" % mongoVersion % Optional,
      "org.mongodb" % "mongodb-driver-async" % mongoVersion % Optional,
      "org.mongodb.scala" %% "mongo-scala-driver" % mongoScalaVersion % Optional,
    ),
  )

lazy val `commons-kafka` = project
  .dependsOn(`commons-core`)
  .settings(
    jvmCommonSettings,
    libraryDependencies ++= Seq(
      "org.apache.kafka" % "kafka-streams" % kafkaVersion,
    ),
  )

lazy val `commons-redis` = project
  .dependsOn(`commons-core`)
  .settings(
    jvmCommonSettings,
    libraryDependencies ++= Seq(
      "com.google.guava" % "guava" % guavaVersion,
      "com.typesafe.akka" %% "akka-actor" % akkaVersion,
      "com.typesafe.scala-logging" %% "scala-logging" % scalaLoggingVersion,
    ),
    parallelExecution in Test := false,
  )

lazy val `commons-spring` = project
  .dependsOn(`commons-core`)
  .settings(
    jvmCommonSettings,
    libraryDependencies ++= Seq(
      "org.springframework" % "spring-context" % springVersion,
      "com.typesafe" % "config" % typesafeConfigVersion,
    ),
  )

lazy val `commons-akka` = project
  .dependsOn(`commons-core`)
  .settings(
    jvmCommonSettings,
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-actor" % akkaVersion,
      "com.typesafe.akka" %% "akka-remote" % akkaVersion,
      "io.monix" %% "monix" % monixVersion,
      "com.typesafe.akka" %% "akka-testkit" % akkaVersion % Test,
      "org.mockito" % "mockito-core" % mockitoVersion % Test,
    ),
  )
