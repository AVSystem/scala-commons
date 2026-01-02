package com.avsystem.commons
package analyzer

import java.io.{PrintWriter, StringWriter}
import scala.tools.nsc.Global
import scala.tools.nsc.Reporting.WarningCategory
import scala.util.control.NonFatal

abstract class AnalyzerRule(val global: Global, val name: String, defaultLevel: Level = Level.Warn) {

  import global._

  var level: Level = defaultLevel
  var argument: String = _

  protected def classType(fullName: String): Type =
    try rootMirror.staticClass(fullName).asType.toType.erasure
    catch {
      case _: ScalaReflectionException => NoType
    }

  protected def analyzeTree(fun: PartialFunction[Tree, Unit])(tree: Tree): Unit =
    try fun.applyOrElse(tree, (_: Tree) => ())
    catch {
      case NonFatal(t) =>
        val sw = new StringWriter
        t.printStackTrace(new PrintWriter(sw))
        reporter.error(tree.pos, s"Analyzer rule $this failed: " + sw.toString)
    }

  private def adjustMsg(msg: String): String = s"[AVS] $msg"

  protected final def report(
    pos: Position,
    message: String,
    category: WarningCategory = WarningCategory.Lint,
    site: Symbol = NoSymbol,
    level: Level = this.level,
  ): Unit =
    level match {
      case Level.Off =>
      case Level.Info => reporter.echo(pos, adjustMsg(message))
      case Level.Warn => currentRun.reporting.warning(pos, adjustMsg(message), category, site)
      case Level.Error => reporter.error(pos, adjustMsg(message))
    }

  def analyze(unit: CompilationUnit): Unit

  override def toString: String =
    getClass.getSimpleName
}

sealed trait Level
object Level {
  case object Off extends Level
  case object Info extends Level
  case object Warn extends Level
  case object Error extends Level
}
