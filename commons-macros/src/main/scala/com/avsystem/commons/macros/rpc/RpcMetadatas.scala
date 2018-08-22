package com.avsystem.commons
package macros.rpc

import scala.annotation.StaticAnnotation
import scala.reflect.{ClassTag, classTag}

trait RpcMetadatas { this: RpcMacroCommons with RpcSymbols with RpcMappings =>

  import c.universe._

  def actualMetadataType(baseMetadataType: Type, realRpcSymbol: RealRpcSymbol, verbatim: Boolean): Res[Type] = {
    val (wildcards, underlying) = baseMetadataType match {
      case ExistentialType(wc, u) if !verbatim => (wc, u)
      case t => (Nil, t)
    }
    val baseMethodResultType = underlying.baseType(TypedMetadataType.typeSymbol).typeArgs.head
    val (realType, realTypeDesc) = realRpcSymbol match {
      case rpc: RealRpcTrait => (rpc.tpe, "RPC type")
      case method: RealMethod => (method.resultType, "method result type")
      case param: RealParam => (param.actualType, "parameter type")
    }

    val result = if (wildcards.isEmpty)
      Some(baseMetadataType).filter(_ => baseMethodResultType =:= realType)
    else determineTypeParams(baseMethodResultType, realType, wildcards)
      .map(typeArgs => underlying.substituteTypes(wildcards, typeArgs))

    result.map(Ok(_)).getOrElse(Fail(
      s"$realTypeDesc $realType is incompatible with required metadata type $baseMetadataType"))
  }

  sealed abstract class MetadataParam(val owner: MetadataConstructor, val symbol: Symbol) extends RpcParam {
    def shortDescription = "metadata parameter"
    def description = s"$shortDescription $nameStr of ${owner.description}"
  }

  class CompositeParam(owner: MetadataConstructor, symbol: Symbol) extends MetadataParam(owner, symbol) {
    val constructor: MetadataConstructor = owner.compositeConstructor(this)
    override def description: String = s"${super.description} at ${owner.description}"

    def pathStr: String = owner.forMethod.atParam.fold(_ => nameStr, cp => s"${cp.pathStr}.$nameStr")
  }

  class MethodMetadataParam(owner: RpcMetadataConstructor, symbol: Symbol)
    extends MetadataParam(owner, symbol) with RawRpcSymbol with ArityParam {

    def allowMulti: Boolean = true
    def allowNamedMulti: Boolean = true
    def allowListedMulti: Boolean = false

    def baseTagTpe: Type = owner.baseMethodTag
    def fallbackTag: FallbackTag = owner.fallbackMethodTag

    val verbatimResult: Boolean =
      annot(RpcEncodingAT).map(_.tpe <:< VerbatimAT).getOrElse(arity.verbatimByDefault)

    if (!(arity.collectedType <:< TypedMetadataType)) {
      reportProblem(s"method metadata type must be a subtype TypedMetadata[_]")
    }

    val (baseParamTag, fallbackParamTag) =
      annot(ParamTagAT).orElse(findAnnotation(arity.collectedType.typeSymbol, ParamTagAT))
        .map(tagSpec).getOrElse(owner.baseParamTag, owner.fallbackParamTag)

    def mappingFor(matchedMethod: MatchedMethod): Res[MethodMetadataMapping] = for {
      mdType <- actualMetadataType(arity.collectedType, matchedMethod.real, verbatimResult)
      constructor = new MethodMetadataConstructor(mdType, Left(this))
      paramMappings <- constructor.paramMappings(matchedMethod)
      tree <- constructor.tryMaterializeFor(matchedMethod, paramMappings)
    } yield MethodMetadataMapping(matchedMethod, this, tree)
  }

  class ParamMetadataParam(owner: MethodMetadataConstructor, symbol: Symbol)
    extends MetadataParam(owner, symbol) with RealParamTarget {

    def pathStr: String = owner.atParam.fold(_ => nameStr, cp => s"${cp.pathStr}.$nameStr")

    def baseTagTpe: Type = owner.containingMethodParam.baseParamTag
    def fallbackTag: FallbackTag = owner.containingMethodParam.fallbackParamTag

    def cannotMapClue = s"cannot map it to $shortDescription $nameStr of ${owner.ownerType}"

    if (!(arity.collectedType <:< TypedMetadataType)) {
      reportProblem(s"type ${arity.collectedType} is not a subtype of TypedMetadata[_]")
    }

    private def metadataTree(matchedParam: MatchedParam, indexInRaw: Int): Res[Tree] = {
      val realParam = matchedParam.real
      val result = for {
        mdType <- actualMetadataType(arity.collectedType, realParam, verbatim)
        tree <- new ParamMetadataConstructor(mdType, Left(this), indexInRaw).tryMaterializeFor(matchedParam)
      } yield tree
      result.mapFailure(msg => s"${realParam.problemStr}: $cannotMapClue: $msg")
    }

    def metadataFor(parser: ParamsParser): Res[Tree] = arity match {
      case _: RpcParamArity.Single =>
        parser.extractSingle(this, metadataTree(_, 0))
      case _: RpcParamArity.Optional =>
        Ok(mkOptional(parser.extractOptional(this, metadataTree(_, 0))))
      case RpcParamArity.Multi(_, true) =>
        parser.extractMulti(this, (mp, i) => metadataTree(mp, i)
          .map(t => q"(${mp.rpcName}, $t)"), named = true).map(mkMulti(_))
      case _: RpcParamArity.Multi =>
        parser.extractMulti(this, metadataTree, named = false).map(mkMulti(_))
    }
  }

  private def primaryConstructor(ownerType: Type, ownerParam: Option[RpcSymbol]): Symbol =
    primaryConstructorOf(ownerType, ownerParam.fold("")(p => s"${p.problemStr}: "))

  sealed abstract class MetadataConstructor(constructor: Symbol) extends RpcMethod {
    def symbol: Symbol = constructor
    def ownerType: Type

    // fallback to annotations on the class itself
    def annot(tpe: Type): Option[Annot] =
      findAnnotation(constructor, tpe) orElse findAnnotation(ownerType.typeSymbol, tpe)

    def shortDescription = "metadata class"
    def description = s"$shortDescription $ownerType"

    def paramByStrategy(paramSym: Symbol, annot: Annot): MetadataParam = annot.tpe match {
      case t if t <:< InferAT => new ImplicitParam(this, paramSym)
      case t if t <:< ReifyAnnotAT => new ReifiedAnnotParam(this, paramSym)
      case t if t <:< ReifyNameAT =>
        val useRpcName = annot.findArg[Boolean](ReifyNameAT.member(TermName("rpcName")), false)
        new ReifiedNameParam(this, paramSym, useRpcName)
      case t if t <:< IsAnnotatedAT =>
        new IsAnnotatedParam(this, paramSym, t.typeArgs.head)
      case t => reportProblem(s"metadata param strategy $t is not allowed here")
    }

    def compositeConstructor(param: CompositeParam): MetadataConstructor

    lazy val paramLists: List[List[MetadataParam]] =
      constructor.typeSignatureIn(ownerType).paramLists.map(_.map { ps =>
        if (findAnnotation(ps, CompositeAT).nonEmpty)
          new CompositeParam(this, ps)
        else findAnnotation(ps, MetadataParamStrategyType).map(paramByStrategy(ps, _)).getOrElse {
          if (ps.isImplicit) new ImplicitParam(this, ps)
          else reportProblem("no metadata param strategy annotation found")
        }
      })

    def constructorCall(argDecls: List[Tree]): Tree =
      q"""
        ..$argDecls
        new $ownerType(...$argLists)
      """

    private def cast[C <: MetadataConstructor : ClassTag]: C = this match {
      case c: C => c
      case _ => throw new Exception(s"Metadata constructor $this is not a ${classTag[C].runtimeClass.getSimpleName}")
    }

    def forRpc: RpcMetadataConstructor = cast[RpcMetadataConstructor]
    def forMethod: MethodMetadataConstructor = cast[MethodMetadataConstructor]
    def forParam: ParamMetadataConstructor = cast[ParamMetadataConstructor]
  }

  case class MethodMetadataMapping(matchedMethod: MatchedMethod, mdParam: MethodMetadataParam, tree: Tree)

  class RpcMetadataConstructor(val ownerType: Type, val atParam: Option[CompositeParam])
    extends MetadataConstructor(primaryConstructor(ownerType, atParam)) with RawRpcSymbol {

    def baseTagTpe: Type = NothingTpe
    def fallbackTag: FallbackTag = FallbackTag.Empty

    override def annot(tpe: Type): Option[Annot] =
      super[MetadataConstructor].annot(tpe)

    val (baseMethodTag, fallbackMethodTag) =
      annot(MethodTagAT).map(tagSpec).getOrElse((NothingTpe, FallbackTag.Empty))
    val (baseParamTag, fallbackParamTag) =
      annot(ParamTagAT).map(tagSpec).getOrElse((NothingTpe, FallbackTag.Empty))

    lazy val methodMdParams: List[MethodMetadataParam] = paramLists.flatten.flatMap {
      case mmp: MethodMetadataParam => List(mmp)
      case rcp: CompositeParam => rcp.constructor.forRpc.methodMdParams
      case _ => Nil
    }

    override def paramByStrategy(paramSym: Symbol, annot: Annot): MetadataParam =
      if (annot.tpe <:< RpcMethodMetadataAT) new MethodMetadataParam(this, paramSym)
      else super.paramByStrategy(paramSym, annot)

    def compositeConstructor(param: CompositeParam): MetadataConstructor =
      new RpcMetadataConstructor(param.actualType, Some(param))

    def methodMappings(rpc: RealRpcTrait): Map[MethodMetadataParam, List[MethodMetadataMapping]] =
      collectMethodMappings(methodMdParams, "metadata parameters", rpc.realMethods)(_.mappingFor(_)).groupBy(_.mdParam)

    def materializeFor(rpc: RealRpcTrait, methodMappings: Map[MethodMetadataParam, List[MethodMetadataMapping]]): Tree = {
      val argDecls = paramLists.flatten.map {
        case rcp: CompositeParam =>
          rcp.localValueDecl(rcp.constructor.forRpc.materializeFor(rpc, methodMappings))
        case dmp: DirectMetadataParam =>
          dmp.localValueDecl(dmp.materializeFor(MatchedRpcTrait(rpc)))
        case mmp: MethodMetadataParam => mmp.localValueDecl {
          val mappings = methodMappings.getOrElse(mmp, Nil)
          mmp.arity match {
            case RpcParamArity.Single(_) => mappings match {
              case Nil => abort(s"no real method found that would match ${mmp.description}")
              case List(m) => m.tree
              case _ => abort(s"multiple real methods match ${mmp.description}")
            }
            case RpcParamArity.Optional(_) => mappings match {
              case Nil => mmp.mkOptional[Tree](None)
              case List(m) => mmp.mkOptional(Some(m.tree))
              case _ => abort(s"multiple real methods match ${mmp.description}")
            }
            case RpcParamArity.Multi(_, _) =>
              mmp.mkMulti(mappings.map(m => q"(${m.matchedMethod.rpcName}, ${m.tree})"))
          }
        }
        case _: ParamMetadataParam => throw new Exception("unexpected ParamMetadataParam")
      }
      constructorCall(argDecls)
    }
  }

  class MethodMetadataConstructor(
    val ownerType: Type,
    val atParam: Either[MethodMetadataParam, CompositeParam]
  ) extends MetadataConstructor(
    primaryConstructor(ownerType, Some(atParam.fold[RpcSymbol](identity, identity)))) {

    val containingMethodParam: MethodMetadataParam =
      atParam.fold(identity, _.owner.forMethod.containingMethodParam)

    lazy val paramMdParams: List[ParamMetadataParam] = paramLists.flatten.flatMap {
      case pmp: ParamMetadataParam => List(pmp)
      case mcp: CompositeParam => mcp.constructor.forMethod.paramMdParams
      case _ => Nil
    }

    override def paramByStrategy(paramSym: Symbol, annot: Annot): MetadataParam =
      if (annot.tpe <:< RpcParamMetadataAT) new ParamMetadataParam(this, paramSym)
      else super.paramByStrategy(paramSym, annot)

    def compositeConstructor(param: CompositeParam): MetadataConstructor =
      new MethodMetadataConstructor(param.actualType, Right(param))

    def paramMappings(matchedMethod: MatchedMethod): Res[Map[ParamMetadataParam, Tree]] =
      collectParamMappings(paramMdParams, "metadata parameter", matchedMethod)(
        (param, parser) => param.metadataFor(parser).map(t => (param, t))).map(_.toMap)

    def tryMaterializeFor(matchedMethod: MatchedMethod, paramMappings: Map[ParamMetadataParam, Tree]): Res[Tree] =
      Res.traverse(paramLists.flatten) {
        case cmp: CompositeParam =>
          cmp.constructor.forMethod.tryMaterializeFor(matchedMethod, paramMappings).map(cmp.localValueDecl)
        case dmp: DirectMetadataParam =>
          dmp.tryMaterializeFor(matchedMethod).map(dmp.localValueDecl)
        case pmp: ParamMetadataParam =>
          Ok(pmp.localValueDecl(paramMappings(pmp)))
        case _: MethodMetadataParam =>
          throw new Exception(s"unexpected MethodMetadataParam")
      }.map(constructorCall)
  }

  class ParamMetadataConstructor(
    val ownerType: Type,
    val atParam: Either[ParamMetadataParam, CompositeParam],
    val indexInRaw: Int
  ) extends MetadataConstructor(
    primaryConstructor(ownerType, Some(atParam.fold[RpcSymbol](identity, identity)))) {

    override def paramByStrategy(paramSym: Symbol, annot: Annot): MetadataParam =
      annot.tpe match {
        case t if t <:< ReifyPositionAT => new ReifiedPositionParam(this, paramSym)
        case t if t <:< ReifyFlagsAT => new ReifiedFlagsParam(this, paramSym)
        case _ => super.paramByStrategy(paramSym, annot)
      }

    def compositeConstructor(param: CompositeParam): MetadataConstructor =
      new ParamMetadataConstructor(param.actualType, Right(param), indexInRaw)

    def tryMaterializeFor(matchedParam: MatchedParam): Res[Tree] =
      Res.traverse(paramLists.flatten) {
        case pcp: CompositeParam =>
          pcp.constructor.forParam.tryMaterializeFor(matchedParam).map(pcp.localValueDecl)
        case dmp: DirectMetadataParam =>
          dmp.tryMaterializeFor(matchedParam).map(dmp.localValueDecl)
        case _: ParamMetadataParam | _: MethodMetadataParam =>
          throw new Exception("unexpected Param/MethodMetadataParam")
      }.map(constructorCall)
  }

  sealed abstract class DirectMetadataParam(owner: MetadataConstructor, symbol: Symbol)
    extends MetadataParam(owner, symbol) {

    def materializeFor(matchedSymbol: Matched): Tree
    def tryMaterializeFor(matchedSymbol: Matched): Res[Tree]
  }

  class ImplicitParam(owner: MetadataConstructor, symbol: Symbol)
    extends DirectMetadataParam(owner, symbol) {

    val checked: Boolean = findAnnotation(symbol, CheckedAT).nonEmpty

    def materializeFor(matchedSymbol: Matched): Tree =
      q"${infer(actualType)}"

    def tryMaterializeFor(matchedSymbol: Matched): Res[Tree] =
      if (checked)
        tryInferCachedImplicit(actualType).map(n => Ok(q"$n"))
          .getOrElse(Fail(s"no implicit value $actualType for $description could be found"))
      else
        Ok(materializeFor(matchedSymbol))
  }

  class ReifiedAnnotParam(owner: MetadataConstructor, symbol: Symbol)
    extends DirectMetadataParam(owner, symbol) with ArityParam {

    def allowMulti: Boolean = true
    def allowNamedMulti: Boolean = false
    def allowListedMulti: Boolean = true

    if (!(arity.collectedType <:< typeOf[StaticAnnotation])) {
      reportProblem(s"${arity.collectedType} is not a subtype of StaticAnnotation")
    }

    def materializeFor(matchedSymbol: Matched): Tree = {
      def validated(annot: Annot): Annot = {
        if (containsInaccessibleThises(annot.tree)) {
          echo(showCode(annot.tree))
          matchedSymbol.real.reportProblem(s"reified annotation contains this-references inaccessible outside RPC trait")
        }
        annot
      }

      val rpcSym = matchedSymbol.real
      arity match {
        case RpcParamArity.Single(annotTpe) =>
          matchedSymbol.annot(annotTpe).map(a => c.untypecheck(validated(a).tree)).getOrElse {
            val msg = s"${rpcSym.problemStr}: cannot materialize value for $description: no annotation of type $annotTpe found"
            q"$RpcUtils.compilationError(${StringLiteral(msg, rpcSym.pos)})"
          }
        case RpcParamArity.Optional(annotTpe) =>
          mkOptional(matchedSymbol.annot(annotTpe).map(a => c.untypecheck(validated(a).tree)))
        case RpcParamArity.Multi(annotTpe, _) =>
          mkMulti(allAnnotations(rpcSym.symbol, annotTpe).map(a => c.untypecheck(validated(a).tree)))
      }
    }

    def tryMaterializeFor(matchedSymbol: Matched): Res[Tree] =
      Ok(materializeFor(matchedSymbol))
  }

  class IsAnnotatedParam(owner: MetadataConstructor, symbol: Symbol, annotTpe: Type)
    extends DirectMetadataParam(owner, symbol) {

    if (!(actualType =:= typeOf[Boolean])) {
      reportProblem("@hasAnnot can only be used on Boolean parameters")
    }

    def materializeFor(matchedSymbol: Matched): Tree =
      q"${matchedSymbol.allAnnots(annotTpe).nonEmpty}"
    def tryMaterializeFor(matchedSymbol: Matched): Res[Tree] =
      Ok(materializeFor(matchedSymbol))
  }

  class ReifiedNameParam(owner: MetadataConstructor, symbol: Symbol, useRpcName: Boolean)
    extends DirectMetadataParam(owner, symbol) {

    if (!(actualType =:= typeOf[String])) {
      reportProblem(s"its type is not String")
    }

    def materializeFor(matchedSymbol: Matched): Tree =
      q"${if (useRpcName) matchedSymbol.rpcName else matchedSymbol.real.nameStr}"

    def tryMaterializeFor(matchedSymbol: Matched): Res[Tree] =
      Ok(materializeFor(matchedSymbol))
  }

  class ReifiedPositionParam(owner: ParamMetadataConstructor, symbol: Symbol)
    extends DirectMetadataParam(owner, symbol) {

    if (!(actualType =:= ParamPositionTpe)) {
      reportProblem("its type is not ParamPosition")
    }

    def materializeFor(matchedParam: Matched): Tree = matchedParam.real match {
      case param: RealParam =>
        q"$ParamPositionObj(${param.index}, ${param.indexOfList}, ${param.indexInList}, ${owner.indexInRaw})"
      case s => throw new Exception(s"not a parameter: $s")
    }

    def tryMaterializeFor(matchedParam: Matched): Res[Tree] =
      Ok(materializeFor(matchedParam))
  }

  class ReifiedFlagsParam(owner: ParamMetadataConstructor, symbol: Symbol)
    extends DirectMetadataParam(owner, symbol) {

    if (!(actualType =:= ParamFlagsTpe)) {
      reportProblem("its type is not ParamFlags")
    }

    def materializeFor(matchedParam: Matched): Tree = {
      val rpcSym = matchedParam.real
      def flag(cond: Boolean, bit: Int) = if (cond) 1 << bit else 0
      val s = rpcSym.symbol.asTerm
      val rawFlags =
        flag(s.isImplicit, 0) |
          flag(s.isByNameParam, 1) |
          flag(isRepeated(s), 2) |
          flag(s.isParamWithDefault, 3) |
          flag(s.isSynthetic, 4)
      q"new $ParamFlagsTpe($rawFlags)"
    }

    def tryMaterializeFor(matchedParam: Matched): Res[Tree] =
      Ok(materializeFor(matchedParam))
  }
}
