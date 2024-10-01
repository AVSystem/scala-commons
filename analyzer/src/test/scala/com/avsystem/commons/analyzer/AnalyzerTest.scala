package com.avsystem.commons
package analyzer

import dotty.tools.dotc.Compiler
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.plugins.Plugin
import org.scalactic.source.Position
import org.scalatest.Assertions

import scala.util.chaining.scalaUtilChainingOps

trait AnalyzerTest:
  analyzer: Assertions =>

  lazy val compiler = new Compiler

  protected val pluginOptions: List[String] = Nil

  protected final def assertErrors(source: String)(using Position): Unit =
    given ctx: Context = compilerContext
    compile(source)
    assert(ctx.reporter.hasErrors)

  end assertErrors

  protected final def assertErrors(errors: Int, source: String)(using  Position): Unit =
    given ctx: Context = compilerContext
    compile(source)
    assert(ctx.reporter.errorCount == errors)

  end assertErrors

  protected final def assertNoErrors(source: String)(using Position): Unit =
    given ctx: Context = compilerContext
    compile(source)
    assert(!ctx.reporter.hasErrors)

  end assertNoErrors

  private def compile(source: String)(using Position, Context): Unit =
    compiler.newRun.compileFromStrings(source :: Nil)

  private def compilerContext: Context =
    val base = new ContextBase:
      override protected def loadRoughPluginsList(using Context): List[Plugin] =
        new AnalyzerPlugin :: super.loadRoughPluginsList

    base.initialCtx.fresh.tap { ctx =>
      ctx.setSetting(ctx.settings.usejavacp, true)
      ctx.setSetting(ctx.settings.pluginOptions, "AVSystemAnalyzer:+_" :: analyzer.pluginOptions)
      base.initialize()(using ctx)
    }

  end compilerContext

end AnalyzerTest
