name := "commons"

version in ThisBuild := "1.3"
scalaVersion in ThisBuild := "2.11.7"
organization in ThisBuild := "com.avsystem.commons"
crossPaths in ThisBuild := false
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
val vaadinVersion = "7.4.6"
val servletApiVersion = "3.1.0"
val springVersion = "4.1.4.RELEASE"
val scalatestVersion = "2.2.5"

val commonSettings = Seq(
  (publishArtifact in packageDoc) := false,
  libraryDependencies += compilerPlugin("com.github.ghik" % "silencer-plugin" % silencerVersion),
  libraryDependencies ++= Seq(
    "com.github.ghik" % "silencer-lib" % silencerVersion,
    "org.scalatest" %% "scalatest" % scalatestVersion % Test
  ),
  ideBasePackages := Seq(organization.value)
)

lazy val commons = project.in(file("."))
  .aggregate(`commons-macros`, `commons-core`, `commons-spring`, `commons-vaadin`, `commons-analyzer`)
  .settings(
    publishArtifact := false
  )

lazy val `commons-macros` = project.in(file("commons-macros"))
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
  )

lazy val `commons-core` = project.in(file("commons-core")).dependsOn(`commons-macros`)
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "com.google.code.findbugs" % "jsr305" % jsr305Version,
      "com.google.guava" % "guava" % guavaVersion
    )
  )

lazy val `commons-spring` = project.in(file("commons-spring")).dependsOn(`commons-core`)
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "org.springframework" % "spring-core" % springVersion,
      "org.springframework" % "spring-beans" % springVersion,
      "org.springframework" % "spring-context" % springVersion,
      "org.springframework" % "spring-context-support" % springVersion
    )
  )

lazy val `commons-vaadin` = project.in(file("commons-vaadin")).dependsOn(`commons-core`)
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "javax.servlet" % "javax.servlet-api" % servletApiVersion,
      "com.vaadin" % "vaadin-server" % vaadinVersion
    )
  )

lazy val `commons-analyzer` = project.in(file("commons-analyzer"))
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value
  )
