package com.avsystem.commons
package analyzer

import com.avsystem.commons.analyzer.AnalyzerTest.ScalaInterpolator
import org.scalactic.source.Position
import org.scalatest.Assertions

import scala.reflect.internal.util.BatchSourceFile
import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.{Global, Settings}

trait AnalyzerTest { this: Assertions =>
  val settings = new Settings
  settings.usejavacp.value = true
  settings.Yrangepos.value = true
  settings.pluginOptions.value ++= List("AVSystemAnalyzer:+_")

  val compiler: Global = new Global(settings) { global =>
    override protected def loadRoughPluginsList(): List[Plugin] =
      new AnalyzerPlugin(global) :: super.loadRoughPluginsList()
  }

  def compile(source: String): Unit = {
    compiler.reporter.reset()
    val run = new compiler.Run
    run.compileSources(List(new BatchSourceFile("test.scala", source)))
  }

  def assertErrors(errors: Int, source: String)(implicit pos: Position): Unit = {
    compile(source)
    assert(compiler.reporter.errorCount == errors)
  }

  def assertNoErrors(source: String)(implicit pos: Position): Unit = {
    compile(source)
    assert(!compiler.reporter.hasErrors)
  }

  implicit final def stringContextToScalaInterpolator(sc: StringContext): ScalaInterpolator = new ScalaInterpolator(sc)
}

object AnalyzerTest {
  final class ScalaInterpolator(private val sc: StringContext) extends AnyVal {
    def scala(args: Any*): String = s"object TopLevel {${sc.s(args*)}}"
  }
}
