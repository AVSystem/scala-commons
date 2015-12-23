package com.avsystem.commons
package analyzer

import scala.tools.nsc.plugins.{Plugin, PluginComponent}
import scala.tools.nsc.{Global, Phase}

final class AnalyzerPlugin(val global: Global) extends Plugin {
  plugin =>

  val rules = List[AnalyzerRule[global.type]](
    new ImportJavaUtil[global.type](global),
    new VarargsAtLeast[global.type](global),
    new DetectSI7046[global.type](global)
  )

  val name = "AVSystemAnalyzer"
  val description = "AVSystem custom Scala static analyzer"
  val components: List[PluginComponent] = List(component)

  private object component extends PluginComponent {
    val global: plugin.global.type = plugin.global
    val runsAfter = List("typer")
    override val runsBefore = List("patmat", "silencer")
    val phaseName = "avsAnalyze"

    import global._

    def newPhase(prev: Phase) = new StdPhase(prev) {
      def apply(unit: CompilationUnit): Unit =
        rules.foreach(_.analyze(unit))
    }
  }

}
