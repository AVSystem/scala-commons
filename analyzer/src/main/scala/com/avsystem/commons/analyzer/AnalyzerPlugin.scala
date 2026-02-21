package com.avsystem.commons
package analyzer

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.plugins.{PluginPhase, StandardPlugin}

class AnalyzerPlugin extends StandardPlugin {

  override val description: String = "AVSystem custom Scala static analyzer"
  val name: String = "AVSystemAnalyzer"
  override def initialize(options: List[String])(using Context): List[PluginPhase] = {
    val rules: List[AnalyzerRule] = List(
      new ImportJavaUtil,
      new VarargsAtLeast,
      new CheckMacroPrivate,
      new ExplicitGenerics,
      new ValueEnumExhaustiveMatch,
      new ShowAst,
      new FindUsages,
      new CheckBincompat,
      new ThrowableObjects,
      new DiscardedMonixTask,
      new NothingAsFunctionArgument,
      new ConstantDeclarations,
      new BasePackage,
      new ImplicitTypes,
      new ImplicitValueClasses,
      new FinalCaseClasses,
      new ImplicitParamDefaults,
      new CatchThrowable,
      new ImplicitFunctionParams,
    )
    parseOptions(options, rules)
    rules.filter(_.level != Level.Off)
  }

  private def parseOptions(options: List[String], rules: List[AnalyzerRule])(using Context): Unit = {
    val byName = rules.map(r => r.name -> r).toMap
    options.foreach { opt =>
      if (opt.startsWith("requireJDK=")) {
        val jdkVersionRegex = opt.substring(opt.indexOf('=') + 1)
        val javaVersion = System.getProperty("java.version", "")
        if (!javaVersion.matches(jdkVersionRegex))
          dotty.tools.dotc.report.error(
            s"This project must be compiled on JDK version matching $jdkVersionRegex but got $javaVersion",
          )
      } else {
        val (level, nameArg) = opt.headOption match {
          case Some('-') => (Level.Off, opt.drop(1))
          case Some('+') => (Level.Error, opt.drop(1))
          case Some('*') => (Level.Info, opt.drop(1))
          case _ => (Level.Warn, opt)
        }
        // Split on ':' to extract per-rule argument, e.g. "findUsages:com.foo.Bar"
        val colonIdx = nameArg.indexOf(':')
        val (ruleName, ruleArg) =
          if (colonIdx >= 0) (nameArg.substring(0, colonIdx), Some(nameArg.substring(colonIdx + 1)))
          else (nameArg, None)
        if (ruleName == "_")
          rules.foreach(_.level = level)
        else
          byName.get(ruleName) match {
            case Some(rule) =>
              rule.level = level
              ruleArg.foreach(arg => rule.argument = Some(arg))
            case None => dotty.tools.dotc.report.error(s"Unrecognized AVS analyzer rule: $name")
          }
      }
    }
  }
}
