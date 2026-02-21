Global / onChangedBuildSource := ReloadOnSourceChanges

import org.scalajs.jsenv.nodejs.NodeJSEnv

// We need to generate slightly different structure for IntelliJ in order to better support ScalaJS cross projects.
// idea.managed property is set by IntelliJ when running SBT (shell or import), idea.runid is set only for IntelliJ's
// SBT shell. In order for this technique to work, you MUST NOT set the "Use the sbt shell for build and import"
// option in IntelliJ's SBT settings.
val forIdeaImport: Boolean = System.getProperty("idea.managed", "false").toBoolean &&
  System.getProperty("idea.runid") == null
val guavaVersion = "33.5.0-jre"
val jsr305Version = "3.0.2"
val scalatestVersion = "3.2.19"
val scalatestplusScalacheckVersion = "3.2.14.0"
val scalacheckVersion = "1.19.0"
val jettyVersion = "12.1.5"
val mongoVersion = "5.6.2"
val springVersion = "6.2.15"
val typesafeConfigVersion = "1.4.5"
val commonsIoVersion = "1.3.2" // test only
val scalaLoggingVersion = "3.9.6"
val pekkoVersion = "1.4.0"
val monixVersion = "3.4.1"
val scalajsBenchmarkVersion = "0.10.0"
val slf4jVersion = "2.0.17" // test only

val scala2Version = "2.13.18"
val scala3Version = "3.8.1"

// for binary compatibility checking
val previousCompatibleVersions: Set[String] = Set("2.2.4")

Global / cancelable := true
Global / excludeLintKeys ++= Set(ideExcludedDirectories, ideOutputDirectory, ideBasePackages, ideSkipProject)

inThisBuild(
  Seq(
    organization := "com.avsystem.commons",
    homepage := Some(url("https://github.com/AVSystem/scala-commons")),
    organizationName := "AVSystem",
    description := "AVSystem commons library for Scala",
    startYear := Some(2015),
    licenses := Vector(License.MIT),
    scmInfo := Some(
      ScmInfo(
        browseUrl = url("https://github.com/AVSystem/scala-commons"),
        connection = "scm:git:git@github.com:AVSystem/scala-commons.git",
        devConnection = Some("scm:git:git@github.com:AVSystem/scala-commons.git"),
      ),
    ),
    developers := List(
      Developer("ddworak", "Dawid Dworak", "d.dworak@avsystem.com", url("https://github.com/ddworak")),
    ),
    scalaVersion := scala3Version,
    githubWorkflowTargetTags ++= Seq("v*"),
    githubWorkflowArtifactUpload := false,
//    githubWorkflowJavaVersions := Seq(JavaSpec.temurin("17"), JavaSpec.temurin("21"), JavaSpec.temurin("25")),
    githubWorkflowJavaVersions := Seq(JavaSpec.temurin("17")),
    githubWorkflowEnv += "JAVA_OPTS" -> "-Dfile.encoding=UTF-8 -Xmx4G",
    githubWorkflowBuildMatrixFailFast := Some(false),
    githubWorkflowBuildPreamble ++= Seq(
      WorkflowStep.Use(
        UseRef.Public("actions", "setup-node", "v4"),
        name = Some("Setup Node.js"),
      ),
      WorkflowStep.Use(
        UseRef.Public("supercharge", "mongodb-github-action", "1.12.1"),
        name = Some("Setup MongoDB"),
        params = Map(
          "mongodb-version" -> "8.0",
          "mongodb-replica-set" -> "test-rs",
        ),
      ),
    ),
    githubWorkflowPublishTargetBranches := Seq(RefPredicate.StartsWith(Ref.Tag("v"))),
    githubWorkflowPublish := Seq(
      WorkflowStep.Sbt(
        List("ci-release"),
        env = Map(
          "PGP_PASSPHRASE" -> "${{ secrets.PGP_PASSPHRASE }}",
          "PGP_SECRET" -> "${{ secrets.PGP_SECRET }}",
          "SONATYPE_PASSWORD" -> "${{ secrets.SONATYPE_PASSWORD }}",
          "SONATYPE_USERNAME" -> "${{ secrets.SONATYPE_USERNAME }}",
        ),
      ),
    ),
  ),
)

def commonSettings: Seq[Def.Setting[?]] = Seq(
  Compile / scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 13)) =>
        Seq(
          "-encoding",
          "utf-8",
          "-Yrangepos",
          "-explaintypes",
          "-feature",
          "-deprecation",
          "-unchecked",
          "-language:implicitConversions",
          "-language:existentials",
          "-language:dynamics",
          "-language:experimental.macros",
          "-language:higherKinds",
          "-Xfatal-warnings",
          "-Xsource:3",
          "-Xlint:-missing-interpolator,-adapted-args,-unused,_",
          "-Ycache-plugin-class-loader:last-modified",
          "-Ycache-macro-class-loader:last-modified",
          "-Xnon-strict-patmat-analysis",
          "-Xlint:-strict-unsealed-patmat",
          "-Ytasty-reader",
          "-Xsource:3",
        )
      case _ =>
        Seq(
          "-deprecation",
          "-feature",
          // "-explain",
          "-unchecked",
          "-language:noAutoTupling",
          "-Vprofile",
          "-Xprint-inline",
          // "-Ycheck:all", // cannot be enabled when scoverage used :///todo: enable
          "-Ycheck:macros",
          // "-Ydebug-error",
          "-Ydebug-flags",
          "-Ydebug-missing-refs",
          "-Yexplain-lowlevel",
          "-Yexplicit-nulls",
          // "-Yprint-debug",
          // "-Xprint:postInlining",
          // "-Xprint-suspension",
          // "-Vprint:typer",
          "-Wsafe-init",
          "-Werror",
          "-experimental",
          "-preview",
          //  "-Yprofile-enabled",
          //  s"-Yprofile-trace:$moduleDir/compile-trace.json",
        )
    }
  },
  Test / scalacOptions := (Compile / scalacOptions).value,
  Compile / doc / sources := Seq.empty, // relying on unidoc
  apiURL := Some(url("http://avsystem.github.io/scala-commons/api")),
  autoAPIMappings := true,
  pomIncludeRepository := { _ => false },
  libraryDependencies ++= Seq(
    "org.scalatest" %%% "scalatest" % scalatestVersion % Test,
    "org.scalacheck" %%% "scalacheck" % scalacheckVersion % Test,
    "org.scalatestplus" %%% "scalacheck-1-16" % scalatestplusScalacheckVersion % Test,
  ),
  ideBasePackages := Seq(organization.value),
  Compile / ideOutputDirectory := Some(target.value.getParentFile / "out/production"),
  Test / ideOutputDirectory := Some(target.value.getParentFile / "out/test"),
  Test / fork := true,
)

val jvmCommonSettings = Seq(
  libraryDependencies ++= Seq(
    "org.apache.commons" % "commons-io" % commonsIoVersion % Test,
    "org.slf4j" % "slf4j-simple" % slf4jVersion % Test,
  ),
  mimaPreviousArtifacts := previousCompatibleVersions.map { previousVersion =>
    organization.value % s"${name.value}_${scalaBinaryVersion.value}" % previousVersion
  },
  Test / jsEnv :=
    new NodeJSEnv(NodeJSEnv.Config().withEnv(Map("RESOURCES_DIR" -> (Test / resourceDirectory).value.absolutePath))),
) ++ commonSettings

val jsCommonSettings = Seq(
//  scalacOptions += {
//    val localDir = (ThisBuild / baseDirectory).value.toURI.toString
//    val githubDir = "https://raw.githubusercontent.com/AVSystem/scala-commons"
//    s"-P:scalajs:mapSourceURI:$localDir->$githubDir/v${version.value}/"
//  },
  jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv(),
  Test / fork := false,
) ++ commonSettings

val noPublishSettings = Seq(
  publish / skip := true,
  mimaPreviousArtifacts := Set.empty,
)

val aggregateProjectSettings =
  noPublishSettings ++ Seq(
    ideSkipProject := true,
    ideExcludedDirectories := Seq(baseDirectory.value),
  )

val CompileAndTest = "compile->compile;test->test"
val OptionalCompileAndTest = "optional->compile;test->test"

lazy val root = project
  .in(file("."))
  .enablePlugins(ScalaUnidocPlugin)
  .aggregate(
    jvm,
    js,
  )
  .settings(
    noPublishSettings,
    name := "commons",
    ideExcludedDirectories := Seq(baseDirectory.value / ".bloop"),
    ScalaUnidoc / unidoc / scalacOptions += "-Ymacro-expand:none",
    ScalaUnidoc / unidoc / unidocProjectFilter := inAnyProject -- inProjects(
      analyzer,
      `core-js`,
//      comprof,
    ),
  )

lazy val jvm = project
  .in(file(".jvm"))
  .aggregate(
    analyzer,
    core,
    jetty,
//    mongo,
    hocon,
//    spring,
//    redis,
  )
  .settings(aggregateProjectSettings)

lazy val js = project
  .in(file(".js"))
  .aggregate(
    `core-js`,
//    `mongo-js`,
  )
  .settings(aggregateProjectSettings)

lazy val analyzer = project
  .dependsOn(core % Test)
  .settings(
    noPublishSettings,
    jvmCommonSettings,
    libraryDependencies ++= Seq(
      "org.scala-lang" %% "scala3-compiler" % scalaVersion.value % "provided",
      "org.scalatest" %% "scalatest" % scalatestVersion % Test,
    ),
    Test / fork := true,
  )

def mkSourceDirs(base: File, scalaBinary: String, conf: String): Seq[File] = Seq(
  base / "src" / conf / "scala",
  base / "src" / conf / s"scala-$scalaBinary",
  base / "src" / conf / "java",
)

def sourceDirsSettings(baseMapper: File => File) = Seq(
  Compile / unmanagedSourceDirectories ++=
    mkSourceDirs(baseMapper(baseDirectory.value), scalaBinaryVersion.value, "main"),
  Test / unmanagedSourceDirectories ++= mkSourceDirs(baseMapper(baseDirectory.value), scalaBinaryVersion.value, "test"),
)

def sameNameAs(proj: Project) =
  if (forIdeaImport) Seq.empty
  else Seq(name := (proj / name).value)

lazy val core = project
  .settings(
    jvmCommonSettings,
    sourceDirsSettings(_ / "jvm"),
    libraryDependencies ++= Seq(
      "com.google.guava" % "guava" % guavaVersion % Optional,
      "io.monix" %% "monix" % monixVersion % Optional,
    ),
  )

lazy val `core-js` = project
  .in(core.base / "js")
  .enablePlugins(ScalaJSPlugin)
  .configure(p => if (forIdeaImport) p.dependsOn(core) else p)
  .settings(
    jsCommonSettings,
    sameNameAs(core),
    sourceDirsSettings(_.getParentFile),
    libraryDependencies ++= Seq(
      "io.monix" %%% "monix" % monixVersion % Optional,
    ),
  )

//todo
//lazy val mongo = project
//  .dependsOn(core % CompileAndTest)
//  .settings(
//    jvmCommonSettings,
//    sourceDirsSettings(_ / "jvm"),
//    libraryDependencies ++= Seq(
//      "com.google.guava" % "guava" % guavaVersion,
//      "com.google.code.findbugs" % "jsr305" % jsr305Version % Optional,
//      "io.monix" %% "monix" % monixVersion,
//      "org.mongodb" % "mongodb-driver-core" % mongoVersion,
//      "org.mongodb" % "mongodb-driver-sync" % mongoVersion % Optional,
//      "org.mongodb" % "mongodb-driver-reactivestreams" % mongoVersion % Optional,
//      ("org.mongodb.scala" %% "mongo-scala-driver" % mongoVersion % Optional).cross(CrossVersion.for3Use2_13),
//    ),
//  )
//
//// only to allow @mongoId & MongoEntity to be usedJS/JVM cross-compiled code
//lazy val `mongo-js` = project
//  .in(mongo.base / "js")
//  .enablePlugins(ScalaJSPlugin)
//  .configure(p => if (forIdeaImport) p.dependsOn(mongo) else p)
//  .dependsOn(`core-js`)
//  .settings(
//    jsCommonSettings,
//    sameNameAs(mongo),
//    sourceDirsSettings(_.getParentFile),
//  )
//
//lazy val redis = project
//  .dependsOn(core % CompileAndTest)
//  .settings(
//    jvmCommonSettings,
//    libraryDependencies ++= Seq(
//      "com.google.guava" % "guava" % guavaVersion,
//      "org.apache.pekko" %% "pekko-stream" % pekkoVersion,
//      "com.typesafe.scala-logging" %% "scala-logging" % scalaLoggingVersion,
//      "io.monix" %% "monix" % monixVersion,
//    ),
//    Test / parallelExecution := false,
//    Compile / scalacOptions += "-Wconf:cat=deprecation:is", // only inform about deprecations due to scheduled removal
//    Test / skip := true,
//  )

lazy val hocon = project
  .dependsOn(core % CompileAndTest)
  .settings(
    jvmCommonSettings,
    libraryDependencies ++= Seq(
      "com.typesafe" % "config" % typesafeConfigVersion,
    ),
  )

lazy val spring = project
  .dependsOn(hocon % CompileAndTest)
  .settings(
    jvmCommonSettings,
    libraryDependencies ++= Seq(
      "org.springframework" % "spring-context" % springVersion,
      "com.google.code.findbugs" % "jsr305" % jsr305Version % Optional,
    ),
  )

lazy val jetty = project
  .dependsOn(core % CompileAndTest)
  .settings(
    jvmCommonSettings,
    libraryDependencies ++= Seq(
      "org.eclipse.jetty" % "jetty-client" % jettyVersion,
      "org.eclipse.jetty.ee10" % "jetty-ee10-servlet" % jettyVersion,
      "com.typesafe.scala-logging" %% "scala-logging" % scalaLoggingVersion,
    ),
  )

lazy val benchmark3 = project
  .in(file("benchmark"))
  .dependsOn(core % CompileAndTest)
  .enablePlugins(JmhPlugin)
  .settings(
    jvmCommonSettings,
    noPublishSettings,
    sourceDirsSettings(_ / "jvm"),
    ideExcludedDirectories := (Jmh / managedSourceDirectories).value,
  )

lazy val benchmark2 = project
  .in(file("benchmark"))
  .enablePlugins(JmhPlugin)
  .settings(
    scalaVersion := scala2Version,
    jvmCommonSettings,
    noPublishSettings,
    sourceDirsSettings(_ / "jvm"),
    ideExcludedDirectories := (Jmh / managedSourceDirectories).value,
    libraryDependencies ++= Seq("com.avsystem.commons" %% "commons-core" % "2.26.0"),
    target := baseDirectory.value / "target" / "scala-2.13",
  )

lazy val `benchmark-compilation3` = project
  .in(file("benchmark-compilation"))
  .dependsOn(core % CompileAndTest)
  .settings(
    jvmCommonSettings,
    noPublishSettings,
  )

lazy val `benchmark-compilation2` = project
  .in(file("benchmark-compilation"))
  .settings(
    scalaVersion := scala2Version,
    jvmCommonSettings,
    noPublishSettings,
    libraryDependencies ++= Seq("com.avsystem.commons" %% "commons-core" % "2.26.0"),
    target := baseDirectory.value / "target" / "scala-2.13",
  )

//lazy val `benchmark-js` = project
//  .in(benchmark.base / "js")
//  .enablePlugins(ScalaJSPlugin, JSDependenciesPlugin)
//  .configure(p => if (forIdeaImport) p.dependsOn(benchmark) else p)
//  .dependsOn(`core-js`)
//  .settings(
//    jsCommonSettings,
//    noPublishSettings,
//    sameNameAs(benchmark),
//    sourceDirsSettings(_.getParentFile),
//    libraryDependencies ++= Seq(
//      "com.github.japgolly.scalajs-benchmark" %%% "benchmark" % scalajsBenchmarkVersion
//    ),
//    scalaJSUseMainModuleInitializer := true,
//  )

//todo: find a replecement
//lazy val comprof = project
//  .disablePlugins(GenerativePlugin)
//  .dependsOn(core)
//  .settings(
//    jvmCommonSettings,
//    noPublishSettings,
//    ideSkipProject := true,
//    addCompilerPlugin("ch.epfl.scala" %% "scalac-profiling" % "1.0.0"),
//    scalacOptions ++= Seq(
//      s"-P:scalac-profiling:sourceroot:${baseDirectory.value}",
//      "-P:scalac-profiling:generate-macro-flamegraph",
//      "-P:scalac-profiling:no-profiledb",
//      "-Xmacro-settings:statsEnabled",
//      "-Ystatistics:typer",
//    ),
//    Compile / sourceGenerators += Def.task {
//      val originalSrc = (core / sourceDirectory).value / "test/scala/com/avsystem/commons/rest/RestTestApi.scala"
//      val originalContent = IO.read(originalSrc)
//      (0 until 100).map { i =>
//        val pkg = f"oa$i%02d"
//        val newContent = originalContent.replace("package rest", s"package rest\npackage $pkg")
//        val newFile = (Compile / sourceManaged).value / pkg / "RestTestApi.scala"
//        IO.write(newFile, newContent)
//        newFile
//      }
//    }.taskValue,
//  )
