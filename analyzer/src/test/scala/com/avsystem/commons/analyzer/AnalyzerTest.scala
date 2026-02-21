package com.avsystem.commons
package analyzer

import dotty.tools.dotc.Compiler
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.plugins.Plugin
import org.scalactic.source.Position
import org.scalatest.Assertions

trait AnalyzerTest { this: Assertions =>

  /** Plugin options applied during compilation. Override to customize per-test. */
  protected def pluginOptions: List[String] = List("AVSystemAnalyzer:+_")

  /**
   * Compile a Scala source string with the AnalyzerPlugin loaded.
   *
   * Creates a fresh ContextBase per invocation to avoid stale compiler state
   * between test cases. The plugin is injected directly via loadRoughPluginsList
   * override, bypassing JAR-based discovery.
   */
  protected def compile(source: String): CompilationResult = {
    val ctxBase = new ContextBase {
      override protected def loadRoughPluginsList(using Context): List[Plugin] =
        new AnalyzerPlugin :: Nil
    }
    given ctx: FreshContext = ctxBase.initialCtx.fresh
    ctx.settings.Yusejavacp.update(true)
    ctx.settings.experimental.update(true)
    ctx.settings.pluginOptions.update(pluginOptions)

    val compiler = new Compiler
    val run = compiler.newRun
    run.compileFromStrings(List(source))

    CompilationResult(
      errorCount = ctx.reporter.errorCount,
      warningCount = ctx.reporter.warningCount,
      errors = ctx.reporter.allErrors.map(_.message),
      warnings = ctx.reporter.allWarnings.map(_.message),
    )
  }

  /**
   * Compile a Scala source string WITHOUT the AnalyzerPlugin.
   *
   * Used by canary tests to prove that diagnostics come from the plugin
   * and not from the standard compiler.
   */
  protected def compileWithoutPlugin(source: String): CompilationResult = {
    val ctxBase = new ContextBase {}
    given ctx: FreshContext = ctxBase.initialCtx.fresh
    ctx.settings.Yusejavacp.update(true)
    ctx.settings.experimental.update(true)

    val compiler = new Compiler
    val run = compiler.newRun
    run.compileFromStrings(List(source))

    CompilationResult(
      errorCount = ctx.reporter.errorCount,
      warningCount = ctx.reporter.warningCount,
      errors = ctx.reporter.allErrors.map(_.message),
      warnings = ctx.reporter.allWarnings.map(_.message),
    )
  }

  def assertErrors(errors: Int, source: String)(using Position): Unit = {
    val result = compile(source)
    assert(
      result.errorCount == errors,
      s"Expected $errors errors, got ${result.errorCount}: ${result.errors.mkString("; ")}",
    )
  }

  def assertNoErrors(source: String)(using Position): Unit = {
    val result = compile(source)
    assert(
      !result.hasErrors,
      s"Expected no errors, got: ${result.errors.mkString("; ")}",
    )
  }

  def assertWarnings(warnings: Int, source: String)(using Position): Unit = {
    val result = compile(source)
    assert(
      result.warningCount == warnings,
      s"Expected $warnings warnings, got ${result.warningCount}: ${result.warnings.mkString("; ")}",
    )
  }

  extension (sc: StringContext) {
    def scala(args: Any*): String = s"object TopLevel {${sc.s(args*)}}"
  }
}

/**
 * Immutable result of a test compilation.
 *
 * Captures both counts and message lists so assertion failures
 * can display the actual diagnostic text for debugging.
 */
final case class CompilationResult(
  errorCount: Int,
  warningCount: Int,
  errors: List[String],
  warnings: List[String],
) {
  def hasErrors: Boolean = errorCount > 0
  def hasWarnings: Boolean = warningCount > 0
}
