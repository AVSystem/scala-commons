name := "commons"

version in Global := "1.0-SNAPSHOT"
scalaVersion in Global := "2.11.7"
organization in Global := "com.avsystem.commons"
crossPaths in Global := false
scalacOptions in Global ++= Seq(
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

publishTo in Global := {
  val name = if (isSnapshot.value) "snapshots" else "releases"
  Some(name at s"http://repo.avsystem.com/libs-$name-local/")
}
publishMavenStyle in Global := true

/* The file with credentials must have following format:
 *
 * realm=Artifactory Realm
 * host=repo.avsystem.com
 * user=<LDAP user>
 * password=<LDAP password>
 *
 */
credentials in Global +=
  Credentials(Path.userHome / ".repo.avsystem.com.credentials")

val silencerVersion = "0.3"
val guavaVersion = "18.0"
val jsr305Version = "3.0.0"
val vaadinVersion = "7.4.6"
val servletApiVersion = "3.1.0"
val scalatestVersion = "2.2.5"

def withSources(modules: ModuleID*) = modules.map(_.withSources())
def test(modules: ModuleID*) = modules.map(_ % Test)
def testWithSources(modules: ModuleID*) = test(withSources(modules: _*): _*)

val commonSettings = Seq(
  (publishArtifact in packageDoc) := false,
  libraryDependencies += compilerPlugin("com.github.ghik" % "silencer-plugin" % silencerVersion),
  libraryDependencies ++= withSources(
    "com.github.ghik" % "silencer-lib" % silencerVersion,
    "com.google.code.findbugs" % "jsr305" % jsr305Version,
    "com.google.guava" % "guava" % guavaVersion
  ),
  libraryDependencies ++= testWithSources(
    "org.scalatest" %% "scalatest" % scalatestVersion
  ),
  ideBasePackages := Seq(organization.value)
)

lazy val commons = project.in(file(".")).aggregate(`commons-macros`, `commons-core`, `commons-vaadin`)
  .settings(
    publishArtifact := false
  )

lazy val `commons-macros` = project.in(file("commons-macros"))
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= withSources(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value
    )
  )

lazy val `commons-core` = project.in(file("commons-core")).dependsOn(`commons-macros`)
  .settings(commonSettings: _*)

lazy val `commons-vaadin` = project.in(file("commons-vaadin")).dependsOn(`commons-core`)
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= withSources(
      "javax.servlet" % "javax.servlet-api" % servletApiVersion,
      "com.vaadin" % "vaadin-server" % vaadinVersion
    )
  )
