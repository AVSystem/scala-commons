package com.avsystem.commons
package macros.rpc

import com.avsystem.commons.macros.meta.MacroSymbols
import com.avsystem.commons.macros.misc.Res

import scala.collection.mutable

private[commons] trait RpcSymbols extends MacroSymbols { this: RpcMacroCommons =>

  import c.universe._

  case class MatchedMethod(
    real: RealMethod,
    raw: RealMethodTarget,
    fallbackTagsUsed: List[FallbackTag]
  ) extends MatchedSymbol {
    type Self = MatchedMethod

    def indexInRaw: Int = 0

    def addFallbackTags(fallbackTags: List[FallbackTag]): MatchedMethod =
      copy(fallbackTagsUsed = fallbackTagsUsed ++ fallbackTags)

    val rawName: String = {
      val prefixes = real.annots(RpcNamePrefixAT, fallback = fallbackTagsUsed.flatMap(_.asList))
        .filter(a => real.overloadIdx > 0 || !a.findArg[Boolean](RpcNameOverloadedOnlyArg, false))
        .map(_.findArg[String](RpcNamePrefixArg))
      val overloadSuffix = if (real.overloadIdx > 0 && raw.mangleOverloads) "_" + real.overloadIdx else ""
      prefixes.mkString("", "", real.rpcName + overloadSuffix)
    }
  }

  case class MatchedTypeParam(
    real: RealTypeParam,
    fallbackTagsUsed: List[FallbackTag],
    matchedOwner: MatchedMethod,
    indexInRaw: Int
  ) extends MatchedSymbol {
    type Self = MatchedTypeParam

    def rawName: String = real.rpcName

    def addFallbackTags(fallbackTags: List[FallbackTag]): MatchedTypeParam =
      copy(fallbackTagsUsed = fallbackTagsUsed ++ fallbackTags)
  }

  case class MatchedParam(
    real: RealParam,
    fallbackTagsUsed: List[FallbackTag],
    matchedOwner: MatchedMethod,
    indexInRaw: Int
  ) extends MatchedSymbol {

    type Self = MatchedParam

    def addFallbackTags(fallbackTags: List[FallbackTag]): MatchedParam =
      copy(fallbackTagsUsed = fallbackTagsUsed ++ fallbackTags)

    def rawName: String = real.rpcName

    val whenAbsent: Tree =
      annot(WhenAbsentAT).fold(EmptyTree) { annot =>
        val annotatedDefault = annot.tree.children.tail.head
        if (!(annotatedDefault.tpe <:< real.actualType)) {
          real.reportProblem(s"expected value of type ${real.actualType} in @whenAbsent annotation, " +
            s"got ${annotatedDefault.tpe.widen}")
        }
        val transformer = new Transformer {
          override def transform(tree: Tree): Tree = tree match {
            case Super(t@This(_), _) if !enclosingClasses.contains(t.symbol) =>
              real.reportProblem(s"illegal super-reference in @whenAbsent annotation")
            case This(_) if tree.symbol == real.owner.owner.symbol => q"${real.owner.owner.safeName}"
            case This(_) if !enclosingClasses.contains(tree.symbol) =>
              real.reportProblem(s"illegal this-reference in @whenAbsent annotation")
            case t => super.transform(t)
          }
        }
        transformer.transform(annotatedDefault)
      }

    val hasDefaultValue: Boolean =
      whenAbsent != EmptyTree || real.symbol.asTerm.isParamWithDefault

    val transientDefault: Boolean =
      hasDefaultValue && annot(TransientDefaultAT).nonEmpty

    def fallbackValueTree: Tree =
      if (whenAbsent != EmptyTree) c.untypecheck(whenAbsent)
      else if (real.symbol.asTerm.isParamWithDefault) defaultValue(false)
      else q"$RpcUtils.missingArg(${matchedOwner.rawName}, $rawName)"

    def transientValueTree: Tree =
      if (real.symbol.asTerm.isParamWithDefault) defaultValue(true)
      else c.untypecheck(whenAbsent)

    private def defaultValue(useThis: Boolean): Tree = {
      val prevListParams = real.owner.realParams.take(real.index - real.indexInList).map(rp => q"${rp.safeName}")
      val prevListParamss = List(prevListParams).filter(_.nonEmpty)
      val realInst = if (useThis) q"this" else q"${real.owner.owner.safeName}"
      q"$realInst.${TermName(s"${real.owner.encodedNameStr}$$default$$${real.index + 1}")}(...$prevListParamss)"
    }
  }

  trait RealRpcSymbol extends MacroSymbol {
    val rpcName: String =
      annot(RpcNameAT).fold(nameStr)(_.findArg[String](RpcNameArg))
  }

  trait RpcApi extends MacroSymbol {
    def tpe: Type
    def symbol: Symbol = tpe.typeSymbol
    def seenFrom: Type = tpe
  }

  trait RpcTrait extends RpcApi {
    if (!symbol.isAbstract || !symbol.isClass) {
      reportProblem(s"it must be an abstract class or trait")
    }
  }

  case class UnmatchedParamError(tagTpe: Type, error: String)

  trait RealMethodTarget extends AritySymbol with TagMatchingSymbol with TagSpecifyingSymbol {
    lazy val verbatimResult: Boolean =
      annot(RpcEncodingAT).map(_.tpe <:< VerbatimAT).getOrElse(arity.verbatimByDefault)

    lazy val mangleOverloads: Boolean =
      annot(MangleOverloadsAT).nonEmpty

    lazy val unmatchedParamErrors: List[UnmatchedParamError] =
      annots(UnmatchedParamAT).map { annot =>
        val tagTpe = annot.tpe.typeArgs.head
        if (!tagSpecs(ParamTagAT).exists(ts => tagTpe <:< ts.baseTagTpe)) {
          reportProblem(s"$tagTpe is not a valid param tag type")
        }
        val error = annot.findArg[String](UnmatchedParamErrorArg)
        UnmatchedParamError(tagTpe, error)
      }

    def errorForUnmatchedParam(param: MacroSymbol): Option[String] = {
      val allFallbackTags = tagSpecs(ParamTagAT).flatMap {
        case BaseTagSpec(baseTagTpe, fallbackTag) =>
          if (fallbackTag.isEmpty || param.annot(baseTagTpe).nonEmpty) None
          else Some(fallbackTag.annotTree)
      }
      unmatchedParamErrors.collectFirst {
        case UnmatchedParamError(tagTpe, error)
          if param.annot(tagTpe, allFallbackTags).nonEmpty => error
      }
    }
  }

  trait RealParamOrTypeParamTarget extends ArityParam with TagMatchingSymbol {
    def allowNamedMulti: Boolean = true
    def allowListedMulti: Boolean = true
    def allowFail: Boolean = true

    def pathStr: String

    val verbatim: Boolean =
      annot(RpcEncodingAT).map(_.tpe <:< VerbatimAT).getOrElse(arity.verbatimByDefault)

    val auxiliary: Boolean =
      annot(AuxiliaryAT).nonEmpty
  }

  trait RealParamTarget extends RealParamOrTypeParamTarget {
    def matchRealParam(matchedMethod: MatchedMethod, realParam: RealParam, indexInRaw: Int): Res[MatchedParam] =
      matchTagsAndFilters(MatchedParam(realParam, Nil, matchedMethod, indexInRaw))
  }

  trait RealTypeParamTarget extends RealParamOrTypeParamTarget {
    def matchRealTypeParam(matchedMethod: MatchedMethod, realTypeParam: RealTypeParam, indexInRaw: Int): Res[MatchedTypeParam] =
      matchTagsAndFilters(MatchedTypeParam(realTypeParam, Nil, matchedMethod, indexInRaw))
  }

  object RawParam {
    def apply(owner: Either[RawMethod, CompositeRawParam], symbol: Symbol): RawParam =
      if (findAnnotation(symbol, MethodNameAT).nonEmpty)
        MethodNameParam(owner, symbol)
      else if (findAnnotation(symbol, CompositeAT).nonEmpty)
        CompositeRawParam(owner, symbol)
      else
        RawValueParam(owner, symbol)
  }

  sealed trait RawParam extends MacroParam {
    val owner: Either[RawMethod, CompositeRawParam]
    val containingRawMethod: RawMethod = owner.fold(identity, _.containingRawMethod)

    def seenFrom: Type = owner.fold(_.seenFrom, _.actualType)
    def safePath: Tree = owner.fold(_ => q"$safeName", _.safeSelect(this))
    def pathStr: String = owner.fold(_ => nameStr, cp => s"${cp.pathStr}.$nameStr")

    def shortDescription = "raw parameter"
    def description = s"$shortDescription $nameStr of ${owner.fold(_.description, _.description)}"
  }

  case class MethodNameParam(owner: Either[RawMethod, CompositeRawParam], symbol: Symbol) extends RawParam {
    if (!(actualType =:= typeOf[String])) {
      reportProblem("@methodName parameter must be of type String")
    }
  }

  case class CompositeRawParam(owner: Either[RawMethod, CompositeRawParam], symbol: Symbol) extends RawParam {
    val constructorSig: Type =
      primaryConstructorOf(actualType, problemStr).typeSignatureIn(actualType)

    val paramLists: List[List[RawParam]] =
      constructorSig.paramLists.map(_.map(RawParam(Right(this), _)))

    def allLeafParams: Iterator[RawParam] = paramLists.iterator.flatten.flatMap {
      case crp: CompositeRawParam => crp.allLeafParams
      case other => Iterator(other)
    }

    def safeSelect(subParam: RawParam): Tree = {
      if (!alternatives(actualType.member(subParam.name)).exists(s => s.isMethod && s.asTerm.isParamAccessor)) {
        subParam.reportProblem(s"it is not a public member and cannot be accessed, turn it into a val")
      }
      q"$safePath.${subParam.name}"
    }
  }

  case class RawValueParam(owner: Either[RawMethod, CompositeRawParam], symbol: Symbol)
    extends RawParam with RealParamTarget {

    def baseTagSpecs: List[BaseTagSpec] = containingRawMethod.tagSpecs(ParamTagAT)
  }

  case class RealTypeParam(owner: RealMethod, symbol: Symbol) extends MacroTypeParam with RealRpcSymbol {
    if (symbol.typeSignature.takesTypeArgs) {
      reportProblem(s"real RPC type parameters must not be higher-kinded")
    }
    if (!(symbol.typeSignature =:= EmptyTypeBounds)) {
      reportProblem(s"real RPC type parameters must not have bounds")
    }

    def seenFrom: Type = owner.seenFrom
    def shortDescription: String = "type parameter"
    def description: String = s"$shortDescription $nameStr of ${owner.description}"

    def typeParamDecl: Tree = typeSymbolToTypeDef(symbol, forMethod = true)
  }

  case class RealParam(owner: RealMethod, symbol: Symbol, index: Int, indexOfList: Int, indexInList: Int)
    extends MacroParam with RealRpcSymbol {

    def seenFrom: Type = owner.seenFrom
    def shortDescription = "parameter"
    def description = s"$shortDescription $nameStr of ${owner.description}"
  }

  case class RawMethod(ownerTrait: RawRpcTrait, symbol: Symbol)
    extends MacroMethod with RealMethodTarget {

    if (sig.typeParams.nonEmpty) {
      reportProblem("raw RPC methods must not be generic")
    }

    def shortDescription = "raw method"
    def description = s"$shortDescription $nameStr of ${ownerTrait.description}"

    def tagSpecifyingOwner: Option[TagSpecifyingSymbol] = Some(ownerTrait)
    def ownerType: Type = ownerTrait.tpe
    def baseTagSpecs: List[BaseTagSpec] = tagSpecs(MethodTagAT)
    def typeParams: List[MacroTypeParam] = Nil

    val arity: MethodArity = MethodArity.fromAnnotation(this)
    val tried: Boolean = annot(TriedAT).nonEmpty

    val rawParams: List[RawParam] = sig.paramLists match {
      case Nil | List(_) => sig.paramLists.flatten.map(RawParam(Left(this), _))
      case _ => reportProblem(s"raw methods cannot take multiple parameter lists")
    }

    val paramLists: List[List[RawParam]] =
      if (sig.paramLists.isEmpty) Nil else List(rawParams)

    def allLeafParams: Iterator[RawParam] = rawParams.iterator.flatMap {
      case crp: CompositeRawParam => crp.allLeafParams
      case other => Iterator(other)
    }

    val allValueParams: List[RawValueParam] =
      allLeafParams.collect({ case rvp: RawValueParam => rvp }).toList

    lazy val methodNameParam: MethodNameParam =
      allLeafParams.collectFirst({ case mnp: MethodNameParam => mnp })
        .getOrElse(reportProblem("no @methodName parameter found on @multi raw method"))

    def rawImpl(caseDefs: List[(String, Tree)]): Tree = {
      val body = arity match {
        case MethodArity.Single => caseDefs match {
          case Nil => abort(s"no real method found that would match $description")
          case List((_, single)) => single
          case _ => abort(s"multiple real methods match $description")
        }
        case MethodArity.Optional => caseDefs match {
          case Nil => q"$RpcUtils.missingOptionalRpc($nameStr)"
          case List((_, single)) => single
          case _ => abort(s"multiple real methods match $description")
        }
        case MethodArity.Multi =>
          val methodNameName = c.freshName(TermName("methodName"))
          q"""
            ${methodNameParam.safePath} match {
              case ..${caseDefs.map({ case (rpcName, tree) => cq"$rpcName => $tree" })}
              case $methodNameName => $RpcUtils.unknownRpc($methodNameName, $nameStr)
            }
           """
      }
      q"def $name(...$paramDecls): $resultType = $body"
    }
  }

  case class RealMethod(owner: RealRpcApi, symbol: Symbol, overloadIdx: Int)
    extends MacroMethod with RealRpcSymbol {

    def ownerType: Type = owner.tpe

    def shortDescription = "method"
    def description = s"$shortDescription $nameStr"

    val typeParams: List[RealTypeParam] =
      sig.typeParams.map(RealTypeParam(this, _))

    val paramLists: List[List[RealParam]] = {
      var index = 0
      var indexOfList = 0
      sig.paramLists.map { ss =>
        var indexInList = 0
        val res = ss.map { s =>
          val res = RealParam(this, s, index, indexOfList, indexInList)
          index += 1
          indexInList += 1
          res
        }
        indexOfList += 1
        res
      }
    }

    val realParams: List[RealParam] = paramLists.flatten
  }

  case class RawRpcTrait(tpe: Type) extends RpcTrait with TagMatchingSymbol with TagSpecifyingSymbol {
    def shortDescription = "raw RPC"
    def description = s"$shortDescription $tpe"

    def tagSpecifyingOwner: Option[TagSpecifyingSymbol] = None
    def baseTagSpecs: List[BaseTagSpec] = Nil

    lazy val rawMethods: List[RawMethod] =
      tpe.members.sorted.iterator.filter(m => m.isTerm && m.isAbstract).map(RawMethod(this, _)).toList
  }

  abstract class RealRpcApi(tpe: Type) extends RpcApi with RealRpcSymbol with SelfMatchedSymbol {
    def description = s"$shortDescription $tpe"

    def isApiMethod(s: TermSymbol): Boolean

    lazy val realMethods: List[RealMethod] = {
      val overloadIndices = new mutable.HashMap[Name, Int]
      tpe.members.sorted.iterator.filter(m => m.isTerm && isApiMethod(m.asTerm)).map { m =>
        val overloadIdx = overloadIndices.getOrElseUpdate(m.name, 0)
        overloadIndices(m.name) = overloadIdx + 1
        RealMethod(this, m, overloadIdx)
      }.toList
    }
  }

  case class RealRpcTrait(tpe: Type) extends RealRpcApi(tpe) with RpcTrait {
    def shortDescription = "real RPC"

    def isApiMethod(s: TermSymbol): Boolean = s.isAbstract
  }

  case class RealApiClass(tpe: Type) extends RealRpcApi(tpe) {
    def shortDescription = "real API"

    def isApiMethod(s: TermSymbol): Boolean =
      s.isPublic && !s.isConstructor && !s.isSynthetic && !isFromToplevelType(s)
  }
}
