package com.avsystem.commons

import com.avsystem.commons.misc.SourceInfo
import com.sun.source.util.SourcePositions

import scala.util.Try

/**
 * Generates a detailed string representation of a symbol during macro expansion.
 *
 * This function produces comprehensive information about a symbol including
 * its owner, flags, names, position, documentation, and structure. It is
 * useful for debugging macro code.
 *
 * @param quotes the Quotes instance
 * @param symbol the symbol to inspect
 * @return a multi-line string with detailed symbol information
 */
private[commons] def symbolInfo(
  using quotes: Quotes,
)(
  symbol: quotes.reflect.Symbol,
)(using quotes.reflect.Printer[quotes.reflect.TypeRepr],
): String =
  s"""
     |$symbol
     |maybeOwner: ${symbol.maybeOwner}
     |flags: ${symbol.flags.show}
     |privateWithin: ${symbol.privateWithin.map(_.show)}
     |protectedWithin: ${symbol.protectedWithin.map(_.show)}
     |name: ${symbol.name}
     |fullName: ${symbol.fullName}
     |pos: ${symbol.pos}
     |docstring: ${symbol.docstring}
     |tree: ${Try(symbol.tree.show).getOrElse("no tree")}
     |annotations: ${symbol.annotations.map(_.show)}
     |isDefinedInCurrentRun: ${symbol.isDefinedInCurrentRun}
     |isLocalDummy: ${symbol.isLocalDummy}
     |isRefinementClass: ${symbol.isRefinementClass}
     |isAliasType: ${symbol.isAliasType}
     |isAnonymousClass: ${symbol.isAnonymousClass}
     |isAnonymousFunction: ${symbol.isAnonymousFunction}
     |isAbstractType: ${symbol.isAbstractType}
     |isClassConstructor: ${symbol.isClassConstructor}
     |isSuperAccessor: ${symbol.isSuperAccessor}
     |isType: ${symbol.isType}
     |isTerm: ${symbol.isTerm}
     |isPackageDef: ${symbol.isPackageDef}
     |isClassDef: ${symbol.isClassDef}
     |isTypeDef: ${symbol.isTypeDef}
     |isValDef: ${symbol.isValDef}
     |isDefDef: ${symbol.isDefDef}
     |isBind: ${symbol.isBind}
     |isNoSymbol: ${symbol.isNoSymbol}
     |exists: ${symbol.exists}
     |declaredFields: ${symbol.declaredFields}
     |fieldMembers: ${symbol.fieldMembers}
     |declaredMethods: ${symbol.declaredMethods}
     |methodMembers: ${symbol.methodMembers}
     |declaredTypes: ${symbol.declaredTypes}
     |typeMembers: ${symbol.typeMembers}
     |declarations: ${symbol.declarations}
     |paramSymss: ${symbol.paramSymss}
     |allOverriddenSymbols: ${symbol.allOverriddenSymbols.toList}
     |primaryConstructor: ${symbol.primaryConstructor}
     |caseFields: ${symbol.caseFields}
     |isTypeParam: ${symbol.isTypeParam}
     |paramVariance: ${symbol.paramVariance.show}
     |signature: ${symbol.signature}
     |moduleClass: ${symbol.moduleClass}
     |companionClass: ${symbol.companionClass}
     |companionModule: ${symbol.companionModule}
     |children: ${symbol.children}
     |typeRef: ${Try(symbol.typeRef.show).getOrElse("no typeRef")}
     |termRef: ${Try(symbol.termRef.show).getOrElse("no termRef")}
     |""".stripMargin

/**
 * Generates a detailed string representation of a type during macro expansion.
 *
 * This function produces comprehensive information about a type including
 * its widened forms, symbols, base classes, and structural properties.
 * It is useful for debugging macro code.
 *
 * @param quotes the Quotes instance
 * @param tpe the type to inspect
 * @return a multi-line string with detailed type information
 */
private[commons] def typeReprInfo(
  using quotes: Quotes,
)(
  tpe: quotes.reflect.TypeRepr,
)(using quotes.reflect.Printer[quotes.reflect.TypeRepr],
): String =
  s"""
     |type: ${tpe.show}
     |raw: $tpe
     |widen: ${tpe.widen.show}
     |widenTermRefByName: ${tpe.widenTermRefByName.show}
     |widenByName: ${tpe.widenByName.show}
     |dealias: ${tpe.dealias.show}
     |dealiasKeepOpaques: ${tpe.dealiasKeepOpaques.show}
     |simplified: ${tpe.simplified.show}
     |classSymbol: ${tpe.classSymbol}
     |typeSymbol: ${tpe.typeSymbol}
     |termSymbol: ${tpe.termSymbol}
     |isSingleton: ${tpe.isSingleton}
     |baseClasses: ${tpe.baseClasses}
     |isFunctionType: ${tpe.isFunctionType}
     |isContextFunctionType: ${tpe.isContextFunctionType}
     |isErasedFunctionType: ${tpe.isErasedFunctionType}
     |isDependentFunctionType: ${tpe.isDependentFunctionType}
     |isTupleN: ${tpe.isTupleN}
     |typeArgs: ${tpe.typeArgs}
     |""".stripMargin

/**
 * Generates a string representation of a tree during macro expansion.
 *
 * This function shows both the structural representation and the short code
 * representation of a tree. It is useful for debugging macro code.
 *
 * @param quotes the Quotes instance
 * @param tree the tree to inspect
 * @return a multi-line string with tree structure and code
 */
private[commons] def treeInfo(using quotes: Quotes)(tree: quotes.reflect.Tree): String = {
  import quotes.reflect.*
  s"""
     |Structure ${Printer.TreeStructure.show(tree)}
     |ShortCode ${Printer.TreeShortCode.show(tree)}
     |""".stripMargin
}
private[commons] def positionInfo(using quotes: Quotes)(pos: quotes.reflect.Position): String =
  s"""
     |start: ${pos.start},
     |end: ${pos.end},
     |startLine: ${pos.startLine},
     |endLine: ${pos.endLine},
     |startColumn: ${pos.startColumn},
     |endColumn: ${pos.endColumn},
     |sourceFile: ${pos.sourceFile},
     |""".stripMargin

inline private[commons] def showAst(inline body: Any) = ${ showAstImpl('{ body }) }

private def showAstImpl(body: Expr[Any])(using quotes: Quotes): Expr[Nothing] = {
  import quotes.reflect.*
  Printer.TreeShortCode.show(body.asTerm.underlyingArgument).dbg
}

inline private[commons] def showRawAst(inline body: Any) = ${ showRawAstImpl('{ body }) }

private def showRawAstImpl(body: Expr[Any])(using quotes: Quotes): Expr[Nothing] = {
  import quotes.reflect.*
  Printer.TreeStructure.show(body.asTerm.underlyingArgument).dbg
}

extension (s: String) {private[commons] def dbg(using quotes: Quotes): Nothing = {
  import quotes.reflect.*
  report.errorAndAbort(s)
}
  private[commons] def info(using quotes: Quotes): String = {
    import quotes.reflect.*
    report.info(s)
    s
  }
}

inline private[commons] def raiseUnsupportedTypeFor[For <: AnyKind, Provided] = ${
  raiseUnsupportedTypeForImpl[For, Provided]
}
private def raiseUnsupportedTypeForImpl[For <: AnyKind: Type, Provided: Type](using quotes: Quotes): Expr[Nothing] = {
  import quotes.reflect.*
  given Printer[TypeRepr] = Printer.TypeReprShortCode
  report.error(s"Unsupported type for ${TypeRepr.of[For].show}: ${TypeRepr.of[Provided].show}")
  '{ ??? }
}

inline private[commons] def raiseCannotDerivedTypeFor[For <: AnyKind, Provided] = ${
  raiseCannotDerivedTypeForImpl[For, Provided]
}
private def raiseCannotDerivedTypeForImpl[For <: AnyKind: Type, Provided: Type](using quotes: Quotes): Expr[Nothing] = {
  import quotes.reflect.*
  given Printer[TypeRepr] = Printer.TypeReprShortCode
  report.error(s"Cannot derive for ${TypeRepr.of[For].show} for ${TypeRepr.of[Provided].show}")
  '{ ??? }
}

inline private[commons] def showTypeRepr[T] = ${ showTypeReprImpl[T] }

private def showTypeReprImpl[T: Type](using quotes: Quotes): Expr[Nothing] = {
  import quotes.reflect.*

  typeReprInfo(TypeRepr.of[T]).dbg
}

private[commons] def wontHappen(using quotes: Quotes)(using pos: SourceInfo) =
  quotes.reflect.report.errorAndAbort(s"This code should never be executed. $pos")
