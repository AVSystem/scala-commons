package com.avsystem.commons.macros

import scala.quoted.*

// Separate package to bypass package-object auto-import of `universalOps`,
// which would otherwise make `Position.sourceCode` recursively resolve to
// `UniversalOps.sourceCode` (this very macro).
object UniversalOpsSourceMacros {
  def sourceCodeImpl[A: Type](a: Expr[A])(using Quotes): Expr[String] =
    import quotes.reflect.*
    val pos = a.asTerm.pos
    val txt = pos.sourceCode.orElse(Position.ofMacroExpansion.sourceCode).getOrElse(
      report.errorAndAbort("source code unavailable at this position", a)
    )
    Expr(txt)

  def withSourceCodeImpl[A: Type](a: Expr[A])(using Quotes): Expr[(A, String)] =
    val src = sourceCodeImpl[A](a)
    '{ ($a, $src) }
}
