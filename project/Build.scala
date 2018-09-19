import sbt.{Project, ProjectReference}

object Build {
  val CompileAndTest = "compile->compile;test->test"

  implicit class projectOps(private val project: Project) extends AnyVal {
    def dependsOnWithTests(deps: ProjectReference*): Project =
      project.dependsOn(deps.map(_ % CompileAndTest): _*)
  }
}
