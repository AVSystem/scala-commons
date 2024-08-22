package com.avsystem.commons
package analyzer

import scala.reflect.internal.util.NoPosition
import scala.tools.nsc.plugins.{Plugin, PluginComponent}
import scala.tools.nsc.{Global, Phase}

final class AnalyzerPlugin(val global: Global) extends Plugin { plugin =>

  override def init(options: List[String], error: String => Unit): Boolean = {
    options.foreach { option =>
      if (option.startsWith("requireJDK=")) {
        val jdkVersionRegex = option.substring(option.indexOf('=') + 1)
        val javaVersion = System.getProperty("java.version", "")
        if (!javaVersion.matches(jdkVersionRegex)) {
          global.reporter.error(NoPosition,
            s"This project must be compiled on JDK version that matches $jdkVersionRegex but got $javaVersion")
        }
      } else {
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
    }
    true
  }

  private lazy val rules = List(
    new ImportJavaUtil(global),
    new VarargsAtLeast(global),
    new CheckMacroPrivate(global),
    new ExplicitGenerics(global),
    new ValueEnumExhaustiveMatch(global),
    new ShowAst(global),
    new FindUsages(global),
    new CheckBincompat(global),
    new Any2StringAdd(global),
    new ThrowableObjects(global),
    new DiscardedMonixTask(global),
    new BadSingletonComponent(global),
    new ConstantDeclarations(global),
    new BasePackage(global),
    new ByNameImplicitParameter(global),
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

    def newPhase(prev: Phase): StdPhase = new StdPhase(prev) {
      def apply(unit: CompilationUnit): Unit =
        rules.foreach(rule => if (rule.level != Level.Off) rule.analyze(unit.asInstanceOf[rule.global.CompilationUnit]))
    }
  }

}
