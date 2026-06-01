package com.avsystem.commons.macros

import scala.quoted.*

object UniversalOpsMacros {
  def showAstImpl[A: Type](a: Expr[A])(using Quotes): Expr[A] =
    import quotes.reflect.*
    report.info(Printer.TreeCode.show(a.asTerm), a)
    a

  def showRawAstImpl[A: Type](a: Expr[A])(using Quotes): Expr[A] =
    import quotes.reflect.*
    report.info(Printer.TreeStructure.show(a.asTerm), a)
    a

  def showSymbolImpl[A: Type](a: Expr[A])(using Quotes): Expr[A] =
    import quotes.reflect.*
    report.info(a.asTerm.symbol.toString, a)
    a

  def showSymbolFullNameImpl[A: Type](a: Expr[A])(using Quotes): Expr[A] =
    import quotes.reflect.*
    report.info(a.asTerm.symbol.fullName, a)
    a

  def showTypeImpl[A: Type](a: Expr[A])(using Quotes): Expr[A] =
    import quotes.reflect.*
    report.info(TypeRepr.of[A].widen.show, a)
    a

  def showRawTypeImpl[A: Type](a: Expr[A])(using Quotes): Expr[A] =
    import quotes.reflect.*
    report.info(Printer.TypeReprStructure.show(TypeRepr.of[A].widen), a)
    a

  def showTypeSymbolImpl[A: Type](a: Expr[A])(using Quotes): Expr[A] =
    import quotes.reflect.*
    report.info(TypeRepr.of[A].typeSymbol.toString, a)
    a

  def showTypeSymbolFullNameImpl[A: Type](a: Expr[A])(using Quotes): Expr[A] =
    import quotes.reflect.*
    report.info(TypeRepr.of[A].typeSymbol.fullName, a)
    a

  private def captureSourceCode[A: Type](a: Expr[A])(using Quotes): Expr[String] =
    import quotes.reflect.*
    val src = Position.ofMacroExpansion.sourceCode
    val txt: String = src match
      case Some(s: String) => s
      case _ => report.errorAndAbort("source code unavailable at this position", a)
    Expr(txt)

  def sourceCodeImpl[A: Type](a: Expr[A])(using Quotes): Expr[String] =
    captureSourceCode[A](a)

  def withSourceCodeImpl[A: Type](a: Expr[A])(using Quotes): Expr[(A, String)] =
    val src = captureSourceCode[A](a)
    '{ ($a, $src) }
}
