package com.avsystem.commons
package analyzer

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.plugins.PluginPhase
import dotty.tools.dotc.typer.TyperPhase
import dotty.tools.dotc.transform.Pickler

/**
 * Base trait for all AVSystem analyzer rules.
 *
 * Each rule is a Scala 3 [[PluginPhase]] (MiniPhase) that runs after the typer
 * and before the pickler. Rules override one or more `transformX` hooks from
 * [[dotty.tools.dotc.transform.MegaPhase.MiniPhase]] to inspect the typed tree and
 * emit diagnostics via the [[report]] helper.
 *
 * Rules MUST return the original tree unchanged from every `transformX` override
 * (analysis-only; no tree modification).
 */
trait AnalyzerRule extends PluginPhase {

  /** Short name used in plugin options, e.g. `catchThrowable` for `-P:AVSystemAnalyzer:+catchThrowable`. */
  val name: String

  /** Current diagnostic level. Mutable so [[AnalyzerPlugin]] can configure it from plugin options. */
  var level: Level = Level.Warn

  /**
   * Optional per-rule argument, e.g. the class name for `findUsages:com.foo.Bar`.
   * Populated by [[AnalyzerPlugin]] when parsing `ruleName:arg` option syntax.
   */
  var argument: Option[String] = None

  /** Unique phase name used by the Scala 3 phase scheduler. Namespaced to avoid conflicts. */
  final def phaseName: String = s"avs.$name"

  /** Run after the typer phase so all symbols and types are fully resolved. */
  override def runsAfter: Set[String] = Set(TyperPhase.name)

  /** Run before the pickler so we operate on the original typed tree (not yet serialised to TASTy). */
  override def runsBefore: Set[String] = Set(Pickler.name)

  /** Emit a diagnostic at the given tree's source position, using the configured [[level]]. */
  protected final def report(tree: tpd.Tree, message: String)(using Context): Unit = level match {
    case Level.Off => ()
    case Level.Info => dotty.tools.dotc.report.echo(s"[AVS] $message", tree.srcPos)
    case Level.Warn => dotty.tools.dotc.report.warning(s"[AVS] $message", tree.srcPos)
    case Level.Error => dotty.tools.dotc.report.error(s"[AVS] $message", tree.srcPos)
  }
}

enum Level {
  case Off, Info, Warn, Error
}
