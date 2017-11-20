package com.avsystem.commons
package analyzer

import scala.tools.nsc.plugins.{Plugin, PluginComponent}
import scala.tools.nsc.{Global, Phase}

final class AnalyzerPlugin(val global: Global) extends Plugin { plugin =>

  val rules = List[AnalyzerRule[global.type]](
    new ImportJavaUtil[global.type](global),
    new VarargsAtLeast[global.type](global),
    new CheckMacroPrivate[global.type](global),
    new ExplicitGenerics[global.type](global),
    new ValueEnumExhaustiveMatch[global.type](global)
  )
  val rulesByName = rules.map(r => (r.name, r)).toMap

  override def init(options: List[String], error: String => Unit): Boolean = {
    options.foreach { option =>
      val level = option.charAt(0) match {
        case '-' => Level.Off
        case '+' => Level.Error
        case _ => Level.Warn
      }
      val name = if (level != Level.Warn) option.drop(1) else option
      if (name == "_") {
        rules.foreach(_.level = level)
      } else rulesByName.get(name) match {
        case Some(rule) => rule.level = level
        case None => error(s"Unrecognized AVS analyzer rule: $name")
      }
    }
    true
  }

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
        rules.foreach(rule => if (rule.level != Level.Off) rule.analyze(unit))
    }
  }

}
