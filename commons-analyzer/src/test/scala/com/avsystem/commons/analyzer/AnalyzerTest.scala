package com.avsystem.commons
package analyzer

import org.scalatest.FunSuite

import scala.reflect.internal.util.BatchSourceFile
import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.{Global, Settings}

/**
 * Author: ghik
 * Created: 21/08/15.
 */
class AnalyzerTest extends FunSuite {
  val settings = new Settings
  settings.usejavacp.value = true
  val compiler = new Global(settings) {
    global =>
    override protected def loadRoughPluginsList(): List[Plugin] =
      new AnalyzerPlugin(global) :: super.loadRoughPluginsList()
  }

  def compile(source: String): Unit = {
    val run = new compiler.Run
    run.compileSources(List(new BatchSourceFile("test.scala", source)))
  }

  def assertErrors(source: String): Unit = {
    compile(source)
    assert(compiler.reporter.hasErrors)
  }

  test("import java.util should be rejected") {
    assertErrors(
      """
        |import java.util
        |
        |object whatever
      """.stripMargin
    )
  }
}
