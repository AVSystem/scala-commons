package com.avsystem.commons.macros

import scala.quoted.*

object ShowMacros:
  // report.info: print + proceed (Scala 2 used c.error as a hack — Scala 3 has a proper info channel)
  def showAstImpl[A: Type](a: Expr[A])(using Quotes): Expr[A] =
    import quotes.reflect.*
    report.info(Printer.TreeCode.show(a.asTerm), a.asTerm.pos)
    a

  def showRawAstImpl[A: Type](a: Expr[A])(using Quotes): Expr[A] =
    import quotes.reflect.*
    report.info(Printer.TreeStructure.show(a.asTerm), a.asTerm.pos)
    a

  def showSymbolImpl[A: Type](a: Expr[A])(using Quotes): Expr[A] =
    import quotes.reflect.*
    report.info(a.asTerm.symbol.toString, a.asTerm.pos)
    a

  def showSymbolFullNameImpl[A: Type](a: Expr[A])(using Quotes): Expr[A] =
    import quotes.reflect.*
    report.info(a.asTerm.symbol.fullName, a.asTerm.pos)
    a

  def showTypeImpl[A: Type](a: Expr[A])(using Quotes): Expr[A] =
    import quotes.reflect.*
    report.info(TypeRepr.of[A].widen.show, a.asTerm.pos)
    a

  def showRawTypeImpl[A: Type](a: Expr[A])(using Quotes): Expr[A] =
    import quotes.reflect.*
    report.info(Printer.TypeReprStructure.show(TypeRepr.of[A].widen), a.asTerm.pos)
    a

  def showTypeSymbolImpl[A: Type](a: Expr[A])(using Quotes): Expr[A] =
    import quotes.reflect.*
    report.info(TypeRepr.of[A].typeSymbol.toString, a.asTerm.pos)
    a

  def showTypeSymbolFullNameImpl[A: Type](a: Expr[A])(using Quotes): Expr[A] =
    import quotes.reflect.*
    report.info(TypeRepr.of[A].typeSymbol.fullName, a.asTerm.pos)
    a

  def sourceCodeImpl[A: Type](a: Expr[A])(using Quotes): Expr[String] =
    import quotes.reflect.*
    val txt = a.asTerm.pos.sourceCode.getOrElse {
      report.errorAndAbort("source code unavailable at this position", a.asTerm.pos)
    }
    Expr(txt)

  def withSourceCodeImpl[A: Type](a: Expr[A])(using Quotes): Expr[(A, String)] =
    val src = sourceCodeImpl[A](a)
    '{ ($a, $src) }
