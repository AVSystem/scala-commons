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
    indexInRaw: Int,
    fallbackTagsUsed: List[FallbackTag]
  ) extends MatchedSymbol {
    type Self = MatchedMethod

    def index: Int = real.index

    def typeParamsInContext: List[MacroTypeParam] =
      real.ownerApi.typeParams ++ real.typeParams

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
    matchedOwner: MatchedSymbol,
    indexInRaw: Int
  ) extends MatchedSymbol {
    type Self = MatchedTypeParam

    def index: Int = real.index
    def rawName: String = real.rpcName
    def typeParamsInContext: List[MacroTypeParam] = Nil

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

    def index: Int = real.index
    def rawName: String = real.rpcName
    def typeParamsInContext: List[MacroTypeParam] = matchedOwner.typeParamsInContext
  }

  trait RealRpcSymbol extends MacroSymbol {
    def index: Int

    val rpcName: String =
      annot(RpcNameAT).fold(nameStr)(_.findArg[String](RpcNameArg))
  }

  trait RpcApi extends MacroTypeSymbol {
    def tpe: Type
    def symbol: Symbol = tpe.typeSymbol
    def seenFrom: Type = tpe

    def safeTermName: TermName = safeName.toTermName
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
    def allowSingle: Boolean = true
    def allowOptional: Boolean = true
    def allowNamedMulti: Boolean = true
    def allowListedMulti: Boolean = true

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
    def matchRealTypeParam(matched: MatchedSymbol, realTypeParam: RealTypeParam, indexInRaw: Int): Res[MatchedTypeParam] =
      matchTagsAndFilters(MatchedTypeParam(realTypeParam, Nil, matched, indexInRaw))
  }

  object RawParam {
    def apply(containingRawMethod: RawMethod, ownerParam: Option[CompositeRawParam], symbol: Symbol): RawParam =
      if (findAnnotation(symbol, MethodNameAT).nonEmpty)
        MethodNameParam(containingRawMethod, ownerParam, symbol)
      else if (findAnnotation(symbol, CompositeAT).nonEmpty)
        CompositeRawParam(containingRawMethod, ownerParam, symbol)
      else
        RawValueParam(containingRawMethod, ownerParam, symbol)
  }

  sealed abstract class RawParam extends MacroParam {
    def containingRawMethod: RawMethod
    def ownerParam: Option[CompositeRawParam]

    def seenFrom: Type = ownerParam.fold(containingRawMethod.seenFrom)(_.actualType)
    def safePath: Tree = ownerParam.fold(q"$safeName": Tree)(_.safeSelect(this))
    def pathStr: String = ownerParam.fold(nameStr)(cp => s"${cp.pathStr}.$nameStr")

    def shortDescription = "raw parameter"
    def description: String =
      s"$shortDescription $nameStr of ${ownerParam.fold(containingRawMethod.description)(_.description)}"
  }

  case class MethodNameParam(
    containingRawMethod: RawMethod,
    ownerParam: Option[CompositeRawParam],
    symbol: Symbol
  ) extends RawParam {
    if (!(actualType =:= typeOf[String])) {
      reportProblem("@methodName parameter must be of type String")
    }
  }

  case class CompositeRawParam(
    containingRawMethod: RawMethod,
    ownerParam: Option[CompositeRawParam],
    symbol: Symbol
  ) extends RawParam {
    val constructorSig: Type =
      primaryConstructorOf(actualType, problemStr).typeSignatureIn(actualType)

    val paramLists: List[List[RawParam]] =
      constructorSig.paramLists.map(_.map(RawParam(containingRawMethod, Some(this), _)))

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

  case class RawValueParam(
    containingRawMethod: RawMethod,
    ownerParam: Option[CompositeRawParam],
    symbol: Symbol
  ) extends RawParam with RealParamTarget {

    def baseTagSpecs: List[BaseTagSpec] = containingRawMethod.tagSpecs(ParamTagAT)
  }

  case class RealTypeParam(owner: RealRpcSymbol, index: Int, symbol: Symbol)
    extends MacroTypeParam with RealRpcSymbol {

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

    val encodingDependency: Boolean =
      annot(EncodingDependencyAT).nonEmpty

    val tparamReferences: List[RealTypeParam] =
      owner.typeParams.filter(tp => actualType.contains(tp.symbol))

    lazy val optionLike: Option[CachedImplicit] =
      annot(OptionalParamAT).map(_ => infer(tq"$OptionLikeCls[$actualType]"))

    lazy val nonOptionalType: Type =
      optionLike.fold(actualType)(optionLikeValueType(_, this))

    def optional: Boolean = optionLike.isDefined
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
      case Nil | List(_) => sig.paramLists.flatten.map(RawParam(this, None, _))
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

  case class RealMethod(ownerApi: RealRpcApi, symbol: Symbol, index: Int, overloadIdx: Int)
    extends MacroMethod with RealRpcSymbol {

    def ownerType: Type = ownerApi.tpe

    def shortDescription = "method"
    def description = s"$shortDescription $nameStr"

    val typeParams: List[RealTypeParam] =
      sig.typeParams.iterator.zipWithIndex.map({ case (s, i) => RealTypeParam(this, i, s) }).toList

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
    val (encodingDeps, regularParams) = realParams.partition(_.encodingDependency)

    val resultTparamReferences: List[RealTypeParam] =
      typeParams.filter(tp => resultType.contains(tp.symbol))
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
    def index: Int = 0

    def forTypeConstructor: RealRpcApi
    def isApiMethod(s: TermSymbol): Boolean
    override def typeParamsInContext: List[MacroTypeParam] = typeParams

    lazy val typeParams: List[RealTypeParam] =
      tpe.typeParams.iterator.zipWithIndex.map({ case (s, i) => RealTypeParam(this, i, s) }).toList

    lazy val realMethods: List[RealMethod] = {
      val overloadIndices = new mutable.HashMap[Name, Int]
      tpe.members.sorted.iterator
        .filter(m => m.isTerm && isApiMethod(m.asTerm)).zipWithIndex
        .map { case (m, idx) =>
          val overloadIdx = overloadIndices.getOrElseUpdate(m.name, 0)
          overloadIndices(m.name) = overloadIdx + 1
          RealMethod(this, m, idx, overloadIdx)
        }.toList
    }
  }

  case class RealRpcTrait(tpe: Type) extends RealRpcApi(tpe) with RpcTrait {
    def shortDescription = "real RPC"

    def forTypeConstructor: RealRpcTrait = RealRpcTrait(tpe.typeConstructor)
    def isApiMethod(s: TermSymbol): Boolean = s.isAbstract
  }

  case class RealApiClass(tpe: Type) extends RealRpcApi(tpe) {
    def shortDescription = "real API"

    def forTypeConstructor: RealApiClass = RealApiClass(tpe.typeConstructor)
    def isApiMethod(s: TermSymbol): Boolean =
      s.isPublic && !s.isConstructor && !s.isSynthetic && !isFromToplevelType(s) &&
        findAnnotation(s, IgnoreAT).isEmpty
  }
}
