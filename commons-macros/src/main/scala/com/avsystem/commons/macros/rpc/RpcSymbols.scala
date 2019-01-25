package com.avsystem.commons
package macros.rpc

import com.avsystem.commons.macros.meta.MacroSymbols
import com.avsystem.commons.macros.misc.Res

import scala.collection.mutable

private[commons] trait RpcSymbols extends MacroSymbols { this: RpcMacroCommons =>

  import c.universe._

  sealed trait Matched extends MatchedSymbol {
    def real: RealRpcSymbol
    def overloaded: Boolean

    val rawName: String = {
      val prefixes = real.annots(RpcNamePrefixAT, fallback = fallbackTagUsed.asList)
        .filter(a => overloaded || !a.findArg[Boolean](RpcNameOverloadedOnlyArg, false))
        .map(_.findArg[String](RpcNamePrefixArg))
      prefixes.mkString("", "", real.rpcName)
    }
  }

  case class MatchedRpcTrait(real: RealRpcTrait) extends Matched {
    def overloaded: Boolean = false
    def indexInRaw: Int = 0
  }

  case class MatchedMethod(real: RealMethod, override val fallbackTagUsed: FallbackTag) extends Matched {
    def overloaded: Boolean = real.overloaded
    def indexInRaw: Int = 0
  }

  case class MatchedParam(
    real: RealParam, override val fallbackTagUsed: FallbackTag, matchedOwner: MatchedMethod, indexInRaw: Int
  ) extends Matched {

    def overloaded: Boolean = false

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

  abstract class RpcTrait(val symbol: Symbol) extends MacroSymbol {
    def tpe: Type
    def seenFrom: Type = tpe

    if (!symbol.isAbstract || !symbol.isClass) {
      reportProblem(s"it must be an abstract class or trait")
    }
  }

  abstract class RpcMethod extends MacroMethod {
    if (sig.typeParams.nonEmpty) {
      // can we relax this?
      reportProblem("RPC methods must not be generic")
    }
  }

  trait RealParamTarget extends ArityParam with TagMatchingSymbol {
    def allowNamedMulti: Boolean = true
    def allowListedMulti: Boolean = true
    def allowFail: Boolean = true

    def pathStr: String

    val verbatim: Boolean =
      annot(RpcEncodingAT).map(_.tpe <:< VerbatimAT).getOrElse(arity.verbatimByDefault)

    val auxiliary: Boolean =
      annot(AuxiliaryAT).nonEmpty

    def cannotMapClue: String

    def matchRealParam(matchedMethod: MatchedMethod, realParam: RealParam, indexInRaw: Int): Res[MatchedParam] =
      for {
        fallbackTag <- matchTag(realParam)
        matchedParam = MatchedParam(realParam, fallbackTag, matchedMethod, indexInRaw)
        _ <- matchFilters(matchedParam)
      } yield matchedParam
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
    val constructorSig: Type = primaryConstructorOf(actualType, problemStr).typeSignatureIn(actualType)

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

    def baseTagTpe: Type = containingRawMethod.baseParamTag
    def fallbackTag: FallbackTag = containingRawMethod.fallbackParamTag

    def cannotMapClue = s"cannot map it to $shortDescription $pathStr of ${containingRawMethod.nameStr}"
  }

  case class RealParam(owner: RealMethod, symbol: Symbol, index: Int, indexOfList: Int, indexInList: Int)
    extends MacroParam with RealRpcSymbol {

    def seenFrom: Type = owner.seenFrom
    def shortDescription = "parameter"
    def description = s"$shortDescription $nameStr of ${owner.description}"
  }

  case class RawMethod(owner: RawRpcTrait, symbol: Symbol)
    extends RpcMethod with TagMatchingSymbol with AritySymbol {

    def shortDescription = "raw method"
    def description = s"$shortDescription $nameStr of ${owner.description}"

    def ownerType: Type = owner.tpe
    def baseTagTpe: Type = owner.baseMethodTag
    def fallbackTag: FallbackTag = owner.fallbackMethodTag

    val arity: MethodArity = MethodArity.fromAnnotation(this)
    val tried: Boolean = annot(TriedAT).nonEmpty

    val verbatimResult: Boolean =
      annot(RpcEncodingAT).map(_.tpe <:< VerbatimAT).getOrElse(arity.verbatimByDefault)

    val (baseParamTag, fallbackParamTag) =
      annot(ParamTagAT).map(tagSpec).getOrElse(owner.baseParamTag, owner.fallbackParamTag)

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

  case class RealMethod(owner: RealRpcTrait, symbol: Symbol, overloaded: Boolean)
    extends RpcMethod with RealRpcSymbol {

    def ownerType: Type = owner.tpe

    def shortDescription = "method"
    def description = s"$shortDescription $nameStr"

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

  case class RawRpcTrait(tpe: Type) extends RpcTrait(tpe.typeSymbol) with TagMatchingSymbol {
    def shortDescription = "raw RPC"
    def description = s"$shortDescription $tpe"

    def baseTagTpe: Type = NothingTpe
    def fallbackTag: FallbackTag = FallbackTag.Empty

    val (baseMethodTag, fallbackMethodTag) =
      annot(MethodTagAT).map(tagSpec).getOrElse((NothingTpe, FallbackTag.Empty))
    val (baseParamTag, fallbackParamTag) =
      annot(ParamTagAT).map(tagSpec).getOrElse((NothingTpe, FallbackTag.Empty))

    lazy val rawMethods: List[RawMethod] =
      tpe.members.sorted.iterator.filter(m => m.isTerm && m.isAbstract).map(RawMethod(this, _)).toList
  }

  case class RealRpcTrait(tpe: Type) extends RpcTrait(tpe.typeSymbol) with RealRpcSymbol with SelfMatchedSymbol {
    def shortDescription = "real RPC"
    def description = s"$shortDescription $tpe"

    lazy val realMethods: List[RealMethod] = {
      val usedRpcNames = new mutable.HashSet[String]
      tpe.members.sorted.iterator.filter(m => m.isTerm && m.isAbstract).map { m =>
        val rm = RealMethod(this, m, overloaded = false)
        if (usedRpcNames.add(rm.rpcName)) rm else rm.copy(overloaded = true)
      }.toList
    }
  }
}
