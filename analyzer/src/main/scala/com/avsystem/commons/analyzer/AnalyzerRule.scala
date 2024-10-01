package com.avsystem.commons
package analyzer

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.plugins.PluginPhase
import dotty.tools.dotc.reporting.{Diagnostic, Message}
import dotty.tools.dotc.transform.{Pickler, Staging}
import dotty.tools.dotc.util.SourcePosition

import scala.compiletime.uninitialized

abstract class AnalyzerRule(val name: String, defaultLevel: Level = Level.Warn) extends PluginPhase:

  import tpd.*

  override val runsAfter: Set[String] = Set(Pickler.name)
  override val runsBefore: Set[String] = Set(Staging.name)

  override val phaseName = s"avsAnalyze$name"
  var level: Level = defaultLevel
  var argument: String = uninitialized
  override def toString: String = getClass.getSimpleName

  inline protected final def report(message: String, tree: Tree)(using Context): Unit =
    report(message, tree.symbol)(using tree.sourcePos)

  protected final def report(
    message: String,
    site: Symbol = NoSymbol
  )(using
    position: SourcePosition,
    ctx: Context
  ): Unit = ctx.reporter.report {
    level match
      case Level.Off =>
        return // awful
      case Level.Info =>
        Diagnostic.Info(adjustMsg(message), position)
      case Level.Warn =>
        Diagnostic.UncheckedWarning(adjustMsg(message), position) // todo not sure if correct type of warning
      case Level.Error =>
        Diagnostic.Error(adjustMsg(message), position)
  }

  private def adjustMsg(msg: String): Message = s"[AVS] $msg".toMessage

end AnalyzerRule

enum Level:
  case Off, Info, Warn, Error

end Level

object Level:

  def fromChar(c: Char): Level = c match
    case '-' => Level.Off
    case '*' => Level.Info
    case '+' => Level.Error
    case _ => Level.Warn

end Level
