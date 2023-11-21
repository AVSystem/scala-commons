import com.github.ghik.sbt.nosbt.ProjectGroup
import com.typesafe.tools.mima.plugin.MimaKeys.*
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport.*
import org.scalajs.jsdependencies.sbtplugin.JSDependenciesPlugin
import org.scalajs.jsenv.nodejs.NodeJSEnv
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.*
import pl.project13.scala.sbt.JmhPlugin
import pl.project13.scala.sbt.JmhPlugin.JmhKeys.*
import sbt.*
import sbt.Keys.*
import sbtghactions.GenerativePlugin
import sbtghactions.GenerativePlugin.autoImport.*
import sbtide.Keys.*
import sbtunidoc.BaseUnidocPlugin.autoImport.{unidoc, unidocProjectFilter}
import sbtunidoc.ScalaUnidocPlugin
import sbtunidoc.ScalaUnidocPlugin.autoImport.ScalaUnidoc
import xerial.sbt.Sonatype.autoImport.sonatypeProfileName

object Commons extends ProjectGroup("commons") {
  // We need to generate slightly different structure for IntelliJ in order to better support ScalaJS cross projects.
  // idea.managed property is set by IntelliJ when running SBT (shell or import), idea.runid is set only for IntelliJ's
  // SBT shell. In order for this technique to work, you MUST NOT set the "Use the sbt shell for build and import"
  // option in IntelliJ's SBT settings.
  val forIdeaImport: Boolean = System.getProperty("idea.managed", "false").toBoolean && System.getProperty("idea.runid") == null

  val guavaVersion = "32.1.3-jre"
  val jsr305Version = "3.0.2"
  val scalatestVersion = "3.2.17"
  val scalatestplusScalacheckVersion = "3.2.14.0"
  val scalacheckVersion = "1.17.0"
  val jettyVersion = "10.0.18"
  val mongoVersion = "4.11.1"
  val springVersion = "6.1.0"
  val typesafeConfigVersion = "1.4.3"
  val commonsIoVersion = "1.3.2" // test only
  val scalaLoggingVersion = "3.9.5"
  val akkaVersion = "2.6.19"
  val monixVersion = "3.4.1"
  val circeVersion = "0.14.5" // benchmark only
  val upickleVersion = "3.1.2" // benchmark only
  val scalajsBenchmarkVersion = "0.10.0"
  val slf4jVersion = "2.0.9" // test only

  // for binary compatibility checking
  val previousCompatibleVersions: Set[String] = Set("2.2.4")

  override def globalSettings: Seq[Def.Setting[_]] = Seq(
    cancelable := true,
    excludeLintKeys ++= Set(ideExcludedDirectories, ideOutputDirectory, ideBasePackages, ideSkipProject),
  )

  override def buildSettings: Seq[Def.Setting[_]] = Seq(
    organization := "com.avsystem.commons",
    homepage := Some(url("https://github.com/AVSystem/scala-commons")),
    organizationName := "AVSystem",
    organizationHomepage := Some(url("http://www.avsystem.com/")),
    description := "AVSystem commons library for Scala",
    startYear := Some(2015),
    licenses := Vector(
      "The MIT License" -> url("https://opensource.org/licenses/MIT"),
    ),
    scmInfo := Some(ScmInfo(
      browseUrl = url("https://github.com/AVSystem/scala-commons.git"),
      connection = "scm:git:git@github.com:AVSystem/scala-commons.git",
      devConnection = Some("scm:git:git@github.com:AVSystem/scala-commons.git"),
    )),
    developers := List(
      Developer("ghik", "Roman Janusz", "r.janusz@avsystem.com", url("https://github.com/ghik")),
    ),

    scalaVersion := "2.13.12",
    compileOrder := CompileOrder.Mixed,

    githubWorkflowTargetTags ++= Seq("v*"),

    githubWorkflowEnv ++= Map(
      "REDIS_VERSION" -> "6.2.12",
    ),
    githubWorkflowArtifactUpload := false,
    githubWorkflowJavaVersions := Seq(JavaSpec.temurin("11"), JavaSpec.temurin("17")),
    githubWorkflowBuildPreamble ++= Seq(
      WorkflowStep.Use(
        UseRef.Public("actions", "cache", "v2"),
        name = Some("Cache Redis"),
        params = Map(
          "path" -> "./redis-${{ env.REDIS_VERSION }}",
          "key" -> "${{ runner.os }}-redis-cache-v2-${{ env.REDIS_VERSION }}"
        )
      ),
      WorkflowStep.Use(
        UseRef.Public("actions", "setup-node", "v2"),
        name = Some("Setup Node.js"),
        params = Map("node-version" -> "12")
      ),
      WorkflowStep.Use(
        UseRef.Public("supercharge", "mongodb-github-action", "1.9.0"),
        name = Some("Setup MongoDB"),
        params = Map(
          "mongodb-version" -> "6.0",
          "mongodb-replica-set" -> "test-rs",
        )
      ),
      WorkflowStep.Run(
        List("./install-redis.sh"),
        name = Some("Setup Redis"),
      )
    ),

    githubWorkflowPublishTargetBranches := Seq(RefPredicate.StartsWith(Ref.Tag("v"))),

    githubWorkflowPublish := Seq(WorkflowStep.Sbt(
      List("ci-release"),
      env = Map(
        "PGP_PASSPHRASE" -> "${{ secrets.PGP_PASSPHRASE }}",
        "PGP_SECRET" -> "${{ secrets.PGP_SECRET }}",
        "SONATYPE_PASSWORD" -> "${{ secrets.SONATYPE_PASSWORD }}",
        "SONATYPE_USERNAME" -> "${{ secrets.SONATYPE_USERNAME }}"
      )
    )),
  )

  override def commonSettings: Seq[Def.Setting[_]] = Seq(
    Compile / scalacOptions ++= Seq(
      "-encoding", "utf-8",
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
      "-Xlint:-missing-interpolator,-adapted-args,-unused,_",
      "-Ycache-plugin-class-loader:last-modified",
      "-Ycache-macro-class-loader:last-modified",
    ),

    Compile / scalacOptions ++= {
      if (scalaBinaryVersion.value == "2.13") Seq(
        "-Xnon-strict-patmat-analysis",
        "-Xlint:-strict-unsealed-patmat"
      ) else Seq.empty
    },

    Test / scalacOptions := (Compile / scalacOptions).value,

    Compile / doc / sources := Seq.empty, // relying on unidoc
    apiURL := Some(url("http://avsystem.github.io/scala-commons/api")),
    autoAPIMappings := true,

    sonatypeProfileName := "com.avsystem",
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
    Test / jsEnv := new NodeJSEnv(NodeJSEnv.Config().withEnv(Map(
      "RESOURCES_DIR" -> (Test / resourceDirectory).value.absolutePath)
    )),
  )

  val jsCommonSettings = Seq(
    scalacOptions += {
      val localDir = (ThisBuild / baseDirectory).value.toURI.toString
      val githubDir = "https://raw.githubusercontent.com/AVSystem/scala-commons"
      s"-P:scalajs:mapSourceURI:$localDir->$githubDir/v${version.value}/"
    },
    jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv(),
    Test / fork := false,
  )

  val noPublishSettings = Seq(
    publish / skip := true,
    mimaPreviousArtifacts := Set.empty,
  )

  val aggregateProjectSettings =
    noPublishSettings ++ Seq(
      ideSkipProject := true,
      ideExcludedDirectories := Seq(baseDirectory.value)
    )

  val CompileAndTest = "compile->compile;test->test"
  val OptionalCompileAndTest = "optional->compile;test->test"

  lazy val root = mkRootProject
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
      ScalaUnidoc / unidoc / unidocProjectFilter :=
        inAnyProject -- inProjects(
          analyzer,
          macros,
          `core-js`,
          comprof,
        ),
    )

  lazy val jvm = mkSubProject.in(file(".jvm"))
    .aggregate(
      analyzer,
      macros,
      core,
      jetty,
      mongo,
      hocon,
      spring,
      redis,
    )
    .settings(aggregateProjectSettings)

  lazy val js = mkSubProject.in(file(".js"))
    .aggregate(
      `core-js`,
      `mongo-js`,
    )
    .settings(aggregateProjectSettings)

  lazy val analyzer = mkSubProject
    .dependsOn(core % Test)
    .settings(
      jvmCommonSettings,
      libraryDependencies ++= Seq(
        "org.scala-lang" % "scala-compiler" % scalaVersion.value,
        "io.monix" %% "monix" % monixVersion % Test,
      ),
    )

  def mkSourceDirs(base: File, scalaBinary: String, conf: String): Seq[File] = Seq(
    base / "src" / conf / "scala",
    base / "src" / conf / s"scala-$scalaBinary",
    base / "src" / conf / "java"
  )

  def sourceDirsSettings(baseMapper: File => File) = Seq(
    Compile / unmanagedSourceDirectories ++=
      mkSourceDirs(baseMapper(baseDirectory.value), scalaBinaryVersion.value, "main"),
    Test / unmanagedSourceDirectories ++=
      mkSourceDirs(baseMapper(baseDirectory.value), scalaBinaryVersion.value, "test"),
  )

  def sameNameAs(proj: Project) =
    if (forIdeaImport) Seq.empty
    else Seq(name := (proj / name).value)

  lazy val macros = mkSubProject.settings(
    jvmCommonSettings,
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  )

  lazy val core = mkSubProject
    .dependsOn(macros)
    .settings(
      jvmCommonSettings,
      sourceDirsSettings(_ / "jvm"),
      libraryDependencies ++= Seq(
        "com.google.code.findbugs" % "jsr305" % jsr305Version % Optional,
        "com.google.guava" % "guava" % guavaVersion % Optional,
        "io.monix" %% "monix" % monixVersion % Optional,
      ),
    )

  lazy val `core-js` = mkSubProject.in(core.base / "js")
    .enablePlugins(ScalaJSPlugin)
    .configure(p => if (forIdeaImport) p.dependsOn(core) else p)
    .dependsOn(macros)
    .settings(
      jsCommonSettings,
      sameNameAs(core),
      sourceDirsSettings(_.getParentFile),
      libraryDependencies ++= Seq(
        "io.monix" %%% "monix" % monixVersion % Optional,
      )
    )

  lazy val mongo = mkSubProject
    .dependsOn(core % CompileAndTest)
    .settings(
      jvmCommonSettings,
      sourceDirsSettings(_ / "jvm"),
      libraryDependencies ++= Seq(
        "com.google.guava" % "guava" % guavaVersion,
        "io.monix" %% "monix" % monixVersion,
        "org.mongodb" % "mongodb-driver-core" % mongoVersion,
        "org.mongodb" % "mongodb-driver-sync" % mongoVersion % Optional,
        "org.mongodb" % "mongodb-driver-reactivestreams" % mongoVersion % Optional,
        "org.mongodb.scala" %% "mongo-scala-driver" % mongoVersion % Optional,
      ),
    )

  // only to allow @mongoId & MongoEntity to be usedJS/JVM cross-compiled code
  lazy val `mongo-js` = mkSubProject.in(mongo.base / "js")
    .enablePlugins(ScalaJSPlugin)
    .configure(p => if (forIdeaImport) p.dependsOn(mongo) else p)
    .dependsOn(`core-js`)
    .settings(
      jsCommonSettings,
      sameNameAs(mongo),
      sourceDirsSettings(_.getParentFile),
    )

  lazy val redis = mkSubProject
    .dependsOn(core % CompileAndTest)
    .settings(
      jvmCommonSettings,
      libraryDependencies ++= Seq(
        "com.google.guava" % "guava" % guavaVersion,
        "com.typesafe.akka" %% "akka-stream" % akkaVersion,
        "com.typesafe.scala-logging" %% "scala-logging" % scalaLoggingVersion,
        "io.monix" %% "monix" % monixVersion,
      ),
      Test / parallelExecution := false,
    )

  lazy val hocon = mkSubProject
    .dependsOn(core % CompileAndTest)
    .settings(
      jvmCommonSettings,
      libraryDependencies ++= Seq(
        "com.typesafe" % "config" % typesafeConfigVersion,
      ),
    )

  lazy val spring = mkSubProject
    .dependsOn(hocon % CompileAndTest)
    .settings(
      jvmCommonSettings,
      libraryDependencies ++= Seq(
        "org.springframework" % "spring-context" % springVersion,
        "com.google.code.findbugs" % "jsr305" % jsr305Version % Optional,
      ),
    )

  lazy val jetty = mkSubProject
    .dependsOn(core % CompileAndTest)
    .settings(
      jvmCommonSettings,
      libraryDependencies ++= Seq(
        "org.eclipse.jetty" % "jetty-client" % jettyVersion,
        "org.eclipse.jetty" % "jetty-server" % jettyVersion,
        "com.typesafe.scala-logging" %% "scala-logging" % scalaLoggingVersion,

        "org.eclipse.jetty" % "jetty-servlet" % jettyVersion % Test,
      ),
    )

  lazy val benchmark = mkSubProject
    .dependsOn(redis, mongo)
    .enablePlugins(JmhPlugin)
    .settings(
      jvmCommonSettings,
      noPublishSettings,
      sourceDirsSettings(_ / "jvm"),
      libraryDependencies ++= Seq(
        "io.circe" %% "circe-core" % circeVersion,
        "io.circe" %% "circe-generic" % circeVersion,
        "io.circe" %% "circe-jawn" % circeVersion,
        "io.circe" %% "circe-parser" % circeVersion,
        "com.lihaoyi" %% "upickle" % upickleVersion,
      ),
      ideExcludedDirectories := (Jmh / managedSourceDirectories).value,
    )

  lazy val `benchmark-js` = mkSubProject.in(benchmark.base / "js")
    .enablePlugins(ScalaJSPlugin, JSDependenciesPlugin)
    .configure(p => if (forIdeaImport) p.dependsOn(benchmark) else p)
    .dependsOn(`core-js`)
    .settings(
      jsCommonSettings,
      noPublishSettings,
      sameNameAs(benchmark),
      sourceDirsSettings(_.getParentFile),
      libraryDependencies ++= Seq(
        "io.circe" %%% "circe-core" % circeVersion,
        "io.circe" %%% "circe-generic" % circeVersion,
        "io.circe" %%% "circe-parser" % circeVersion,
        "com.lihaoyi" %%% "upickle" % upickleVersion,
        "com.github.japgolly.scalajs-benchmark" %%% "benchmark" % scalajsBenchmarkVersion,
      ),
      scalaJSUseMainModuleInitializer := true,
    )

  lazy val comprof = mkSubProject
    .disablePlugins(GenerativePlugin)
    .dependsOn(core)
    .settings(
      jvmCommonSettings,
      noPublishSettings,
      ideSkipProject := true,
      addCompilerPlugin("ch.epfl.scala" %% "scalac-profiling" % "1.0.0"),
      scalacOptions ++= Seq(
        s"-P:scalac-profiling:sourceroot:${baseDirectory.value}",
        "-P:scalac-profiling:generate-macro-flamegraph",
        "-P:scalac-profiling:no-profiledb",
        "-Xmacro-settings:statsEnabled",
        "-Ystatistics:typer",
      ),
      Compile / sourceGenerators += Def.task {
        val originalSrc = (core / sourceDirectory).value /
          "test/scala/com/avsystem/commons/rest/RestTestApi.scala"
        val originalContent = IO.read(originalSrc)
        (0 until 100).map { i =>
          val pkg = f"oa$i%02d"
          val newContent = originalContent.replace("package rest", s"package rest\npackage $pkg")
          val newFile = (Compile / sourceManaged).value / pkg / "RestTestApi.scala"
          IO.write(newFile, newContent)
          newFile
        }
      }.taskValue
    )
}
