import com.github.ghik.sbt.nosbt.ProjectGroup
import com.typesafe.tools.mima.core.*
import com.typesafe.tools.mima.core.ProblemFilters.*
import com.typesafe.tools.mima.plugin.MimaPlugin.autoImport.*
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

object Commons extends ProjectGroup("commons") {
  // We need to generate slightly different structure for IntelliJ in order to better support ScalaJS cross projects.
  // idea.managed property is set by IntelliJ when running SBT (shell or import), idea.runid is set only for IntelliJ's
  // SBT shell. In order for this technique to work, you MUST NOT set the "Use the sbt shell for build and import"
  // option in IntelliJ's SBT settings.
  val forIdeaImport: Boolean = System.getProperty("idea.managed", "false").toBoolean &&
    System.getProperty("idea.runid") == null

  val scala3Version = "3.8.2"
  val madeVersion = "0.1.1"

  val guavaVersion = "33.6.0-jre"
  val jsr305Version = "3.0.2"
  val scalatestVersion = "3.2.20"
  val scalatestplusScalacheckVersion = "3.2.14.0"
  val scalacheckVersion = "1.19.0"
  val mongoVersion = "5.7.1"
  val jettyVersion = "12.1.9"
  val typesafeConfigVersion = "1.4.8"
  val commonsIoVersion = "1.3.2" // test only
  val scalaLoggingVersion = "3.9.6"
  val pekkoVersion = "1.4.0"
  val monixVersion = "3.4.1"
  val scalajsBenchmarkVersion = "0.10.0"
  val slf4jVersion = "2.0.18" // test only

  val previousCompatibleVersions: Set[String] =
    Set("2.21.0", "2.22.0", "2.23.0", "2.23.1", "2.24.0", "2.25.0", "2.26.0", "2.27.0", "2.27.1")

  override def globalSettings: Seq[Def.Setting[_]] = Seq(
    cancelable := true,
    excludeLintKeys ++= Set(ideExcludedDirectories, ideOutputDirectory, ideBasePackages, ideSkipProject),
  )

  override def buildSettings: Seq[Def.Setting[_]] = Seq(
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
      )
    ),
    developers := List(
      Developer("ddworak", "Dawid Dworak", "d.dworak@avsystem.com", url("https://github.com/ddworak"))
    ),
    scalaVersion := scala3Version,
    githubWorkflowTargetTags ++= Seq("v*"),
    githubWorkflowArtifactUpload := false,
    githubWorkflowScalaVersions := Seq(scala3Version),
    githubWorkflowJavaVersions := Seq(
      JavaSpec.temurin("17"),
      JavaSpec.temurin("21"),
      JavaSpec.temurin("25"),
    ),
    githubWorkflowEnv += "JAVA_OPTS" -> "-Dfile.encoding=UTF-8 -Xmx4G",
    githubWorkflowBuildMatrixFailFast := Some(false),
    githubWorkflowBuild := Seq(
      WorkflowStep.Sbt(
        List("compile", "Test/compile"),
        name = Some("Build"),
      )
    ),
    githubWorkflowAddedJobs += WorkflowJob(
      id = "scalafmt",
      name = "Scalafmt Check",
      scalas = List(scalaVersion.value),
      javas = List(JavaSpec.temurin("21")),
      steps = githubWorkflowJobSetup.value.toList :+ WorkflowStep.Sbt(
        List("scalafmtCheckAll", "scalafmtSbtCheck"),
        name = Some("Check Scala formatting"),
      ),
    ),
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
      )
    ),
  )

  override def commonSettings: Seq[Def.Setting[_]] = Seq(
    Compile / scalacOptions ++= Seq(
      "-encoding",
      "utf-8",
      "-explain-types",
      "-feature",
      "-deprecation",
      "-unchecked",
      "-language:implicitConversions",
      "-language:existentials",
      "-language:dynamics",
      "-language:higherKinds",
      // TODO[scala3-port]: enable -Werror after warnings clean
      // "-Werror",
    ),
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
    mimaPreviousArtifacts := previousCompatibleVersions.map(v => organization.value %%% moduleName.value % v),
  )

  val jvmCommonSettings = Seq(
    libraryDependencies ++= Seq(
      "org.apache.commons" % "commons-io" % commonsIoVersion % Test,
      "org.slf4j" % "slf4j-simple" % slf4jVersion % Test,
    ),
    Test / jsEnv :=
      new NodeJSEnv(NodeJSEnv.Config().withEnv(Map("RESOURCES_DIR" -> (Test / resourceDirectory).value.absolutePath))),
  )

  val jsCommonSettings = Seq(
    scalacOptions += {
      val localDir = (ThisBuild / baseDirectory).value.toURI.toString
      val githubDir = "https://raw.githubusercontent.com/AVSystem/scala-commons"
      s"-scalajs-mapSourceURI:$localDir->$githubDir/v${version.value}/"
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
      ideExcludedDirectories := Seq(baseDirectory.value),
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
      ScalaUnidoc / unidoc / unidocProjectFilter := inAnyProject -- inProjects(
        // analyzer, // TODO[scala3-port]: re-enable when module restored
        `core-js`,
        comprof,
      ),
    )

  lazy val jvm = mkSubProject
    .in(file(".jvm"))
    .aggregate(
      // TODO[scala3-port]: Scala 2 compiler plugin; restore as Scala 3 plugin (L)
      // analyzer,
      core,
      // TODO[scala3-port]: ee10 servlet wrapper (M)
      // jetty,
      mongo,
      hocon,
    )
    .settings(aggregateProjectSettings)

  lazy val js = mkSubProject
    .in(file(".js"))
    .aggregate(
      `core-js`,
      `mongo-js`,
    )
    .settings(aggregateProjectSettings)

  // TODO[scala3-port]: analyzer module — Scala 2 compiler plugin; restore as Scala 3 plugin or formally drop (L)
  // lazy val analyzer = mkSubProject
  //   .dependsOn(core % Test)
  //   .settings(
  //     jvmCommonSettings,
  //     libraryDependencies ++= Seq(
  //       "org.scala-lang" % "scala-compiler" % scalaVersion.value,
  //       "io.monix" %% "monix" % monixVersion % Test,
  //     ),
  //     mimaBinaryIssueFilters ++= Seq(
  //       exclude[DirectMissingMethodProblem]("com.avsystem.commons.analyzer.AnalyzerRule.report")
  //     ),
  //   )

  def mkSourceDirs(base: File, conf: String): Seq[File] = Seq(
    base / "src" / conf / "scala",
    base / "src" / conf / "java",
  )

  def sourceDirsSettings(baseMapper: File => File) = Seq(
    Compile / unmanagedSourceDirectories ++= mkSourceDirs(baseMapper(baseDirectory.value), "main"),
    Test / unmanagedSourceDirectories ++= mkSourceDirs(baseMapper(baseDirectory.value), "test"),
  )

  def sameNameAs(proj: Project) =
    if (forIdeaImport) Seq.empty
    else Seq(name := (proj / name).value)

  val coreMimaFilters: Seq[ProblemFilter] = Seq(
    // Commit ade8d4a8: OrderingOps removed (superseded by stdlib equivalents).
    exclude[DirectMissingMethodProblem]("com.avsystem.commons.SharedExtensions.orderingOps"),
    exclude[DirectMissingMethodProblem]("com.avsystem.commons.SharedExtensionsUtils.orderingOps"),
    exclude[DirectMissingMethodProblem]("com.avsystem.commons.package.orderingOps"),
    // Commit ade8d4a8: IteratorOps.distinct / distinctBy removed (superseded by stdlib equivalents).
    exclude[DirectMissingMethodProblem]("com.avsystem.commons.SharedExtensionsUtils#IteratorOps.distinct"),
    exclude[DirectMissingMethodProblem]("com.avsystem.commons.SharedExtensionsUtils#IteratorOps.distinctBy"),
    exclude[DirectMissingMethodProblem]("com.avsystem.commons.SharedExtensionsUtils#IteratorOps.distinct$extension"),
    exclude[DirectMissingMethodProblem]("com.avsystem.commons.SharedExtensionsUtils#IteratorOps.distinctBy$extension"),
  )

  lazy val core = mkSubProject.settings(
    jvmCommonSettings,
    sourceDirsSettings(_ / "jvm"),
    libraryDependencies ++= Seq(
      "com.google.guava" % "guava" % guavaVersion % Optional,
      "io.monix" %% "monix" % monixVersion % Optional,
      "io.github.halotukozak" %% "made" % madeVersion,
    ),
    mimaBinaryIssueFilters ++= coreMimaFilters,
  )

  lazy val `core-js` = mkSubProject
    .in(core.base / "js")
    .enablePlugins(ScalaJSPlugin)
    .configure(p => if (forIdeaImport) p.dependsOn(core) else p)
    .settings(
      jsCommonSettings,
      sameNameAs(core),
      sourceDirsSettings(_.getParentFile),
      libraryDependencies ++= Seq(
        "io.monix" %%% "monix" % monixVersion % Optional
      ),
      mimaBinaryIssueFilters ++= coreMimaFilters,
    )

  lazy val mongo = mkSubProject
    .dependsOn(core % CompileAndTest)
    .settings(
      jvmCommonSettings,
      sourceDirsSettings(_ / "jvm"),
      libraryDependencies ++= Seq(
        "com.google.guava" % "guava" % guavaVersion,
        "com.google.code.findbugs" % "jsr305" % jsr305Version % Optional,
        "io.monix" %% "monix" % monixVersion,
        "org.mongodb" % "mongodb-driver-core" % mongoVersion,
        "org.mongodb" % "mongodb-driver-sync" % mongoVersion % Optional,
        "org.mongodb" % "mongodb-driver-reactivestreams" % mongoVersion % Optional,
      ),
      mimaBinaryIssueFilters ++= Seq(
        // PR #827 (commit 09c12e2d): dropped the deprecated `mongo-scala-driver`
        // dependency along with the helpers under `com.avsystem.commons.mongo.scala`.
        exclude[MissingClassProblem]("com.avsystem.commons.mongo.scala.GenCodecCollection"),
        exclude[MissingClassProblem]("com.avsystem.commons.mongo.scala.GenCodecCollection$"),
        exclude[MissingClassProblem]("com.avsystem.commons.mongo.scala.MongoScalaObservableExtensions"),
        exclude[MissingClassProblem]("com.avsystem.commons.mongo.scala.MongoScalaObservableExtensions$"),
        exclude[MissingClassProblem](
          "com.avsystem.commons.mongo.scala.MongoScalaObservableExtensions$MongoObservableOps"
        ),
        exclude[MissingClassProblem](
          "com.avsystem.commons.mongo.scala.MongoScalaObservableExtensions$MongoObservableOps$"
        ),
        // Commit 11e6d7ef (released in 2.25.0): listDatabases/listCollections were
        // split into listRaw* / listTyped* variants. Document-returning overloads
        // got deprecated stubs but the typed (`[T: GenCodec]`) overloads were
        // removed outright; this is an intentional API change.
        exclude[DirectMissingMethodProblem]("com.avsystem.commons.mongo.typed.TypedMongoClient.listDatabases"),
        exclude[DirectMissingMethodProblem]("com.avsystem.commons.mongo.typed.TypedMongoDatabase.listCollections"),
      ),
    )

  // only to allow @mongoId & MongoEntity to be usedJS/JVM cross-compiled code
  lazy val `mongo-js` = mkSubProject
    .in(mongo.base / "js")
    .enablePlugins(ScalaJSPlugin)
    .configure(p => if (forIdeaImport) p.dependsOn(mongo) else p)
    .dependsOn(`core-js`)
    .settings(
      jsCommonSettings,
      sameNameAs(mongo),
      sourceDirsSettings(_.getParentFile),
    )

  lazy val hocon = mkSubProject
    .dependsOn(core % CompileAndTest)
    .settings(
      jvmCommonSettings,
      libraryDependencies ++= Seq(
        "com.typesafe" % "config" % typesafeConfigVersion
      ),
    )

  lazy val jetty = mkSubProject
    .dependsOn(core % CompileAndTest)
    .settings(
      jvmCommonSettings,
      libraryDependencies ++= Seq(
        "org.eclipse.jetty" % "jetty-client" % jettyVersion,
        "org.eclipse.jetty.ee10" % "jetty-ee10-servlet" % jettyVersion,
        "com.typesafe.scala-logging" %% "scala-logging" % scalaLoggingVersion,
      ),
    )

  lazy val benchmark = mkSubProject
    .dependsOn(mongo)
    .enablePlugins(JmhPlugin)
    .settings(
      jvmCommonSettings,
      noPublishSettings,
      sourceDirsSettings(_ / "jvm"),
      ideExcludedDirectories := (Jmh / managedSourceDirectories).value,
    )

  lazy val `benchmark-js` = mkSubProject
    .in(benchmark.base / "js")
    .enablePlugins(ScalaJSPlugin, JSDependenciesPlugin)
    .configure(p => if (forIdeaImport) p.dependsOn(benchmark) else p)
    .dependsOn(`core-js`)
    .settings(
      jsCommonSettings,
      noPublishSettings,
      sameNameAs(benchmark),
      sourceDirsSettings(_.getParentFile),
      libraryDependencies ++= Seq(
        "com.github.japgolly.scalajs-benchmark" %%% "benchmark" % scalajsBenchmarkVersion
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
        val originalSrc = (core / sourceDirectory).value / "test/scala/com/avsystem/commons/rest/RestTestApi.scala"
        val originalContent = IO.read(originalSrc)
        (0 until 100).map { i =>
          val pkg = f"oa$i%02d"
          val newContent = originalContent.replace("package rest", s"package rest\npackage $pkg")
          val newFile = (Compile / sourceManaged).value / pkg / "RestTestApi.scala"
          IO.write(newFile, newContent)
          newFile
        }
      }.taskValue,
    )
}
