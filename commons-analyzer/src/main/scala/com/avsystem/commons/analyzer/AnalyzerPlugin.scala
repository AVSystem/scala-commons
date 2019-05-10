package com.avsystem.commons
package analyzer

import scala.tools.nsc.plugins.{Plugin, PluginComponent}
import scala.tools.nsc.{Global, Phase}

final class AnalyzerPlugin(val global: Global) extends Plugin { plugin =>

  override def init(options: List[String], error: String => Unit): Boolean = {
    options.foreach { option =>
      val level = option.charAt(0) match {
        case '-' => Level.Off
        case '*' => Level.Info
        case '+' => Level.Error
        case _ => Level.Warn
      }
      val nameArg = if (level != Level.Warn) option.drop(1) else option
      if (nameArg == "_") {
        rules.foreach(_.level = level)
      } else {
        val (name, arg) = nameArg.split(":", 2) match {
          case Array(n, a) => (n, a)
          case Array(n) => (n, null)
        }
        rulesByName.get(name) match {
          case Some(rule) =>
            rule.level = level
            rule.argument = arg
          case None =>
            error(s"Unrecognized AVS analyzer rule: $name")
        }
      }
    }
    true
  }

  private lazy val rules = List[AnalyzerRule[global.type]](
    new ImportJavaUtil[global.type](global),
    new VarargsAtLeast[global.type](global),
    new CheckMacroPrivate[global.type](global),
    new ExplicitGenerics[global.type](global),
    new ValueEnumExhaustiveMatch[global.type](global),
    new ShowAst[global.type](global),
    new FindUsages[global.type](global),
    new CheckBincompat[global.type](global),
    new Any2StringAdd[global.type](global)
  )
  private lazy val rulesByName = rules.map(r => (r.name, r)).toMap

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
