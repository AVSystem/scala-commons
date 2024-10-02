package com.avsystem.commons
package analyzer

import dotty.tools.dotc.core.Contexts.{Context, ctx}
import dotty.tools.dotc.plugins.{PluginPhase, StandardPlugin}
import dotty.tools.dotc.reporting.Diagnostic
import dotty.tools.dotc.util.NoSourcePosition

final class AnalyzerPlugin extends StandardPlugin:

  private lazy val rules = List(
    new ImportJavaUtil,
    new ExplicitGenerics
  )

  override val description = "AVSystem custom Scala static analyzer"
  val name = "AVSystemAnalyzer"

  override def initialize(options: List[String])(using Context): List[PluginPhase] =
    parseOptions(options)
    rules

  end initialize

  private def parseOptions(options: List[String])(using Context): Unit =
    lazy val rulesByName = rules.map(r => (r.name, r)).toMap
    options.foreach { option =>
      if option.startsWith("requireJDK=") then validateJdk(option)
      else
        val level = Level.fromChar(option.charAt(0))
        val nameArg = if level != Level.Warn then option.drop(1) else option
        if nameArg == "_" then rules.foreach(_.level = level)
        else
          val (name, arg) = nameArg.split(":", 2) match
            case Array(n, a) => (n, a)
            case Array(n)    => (n, null)
          rulesByName.get(name) match
            case Some(rule) =>
              rule.level = level
              rule.argument = arg
            case None =>
              ctx.reporter.report(
                Diagnostic.Error(
                  s"Unrecognized AVS analyzer rule: $name",
                  NoSourcePosition
                )
              )
          end match

        end if
    }

  end parseOptions

  private def validateJdk(option: String)(using Context): Unit =
    val jdkVersionRegex = option.substring(option.indexOf('=') + 1)
    val javaVersion = System.getProperty("java.version", "")
    if !javaVersion.matches(jdkVersionRegex) then
      ctx.reporter.report(
        Diagnostic.Error(
          s"This project must be compiled on JDK version that matches $jdkVersionRegex but got $javaVersion",
          NoSourcePosition
        )
      )

    end if

  end validateJdk

end AnalyzerPlugin
