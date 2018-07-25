package com.avsystem.commons
package macros.rpc

import scala.annotation.StaticAnnotation

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

  sealed abstract class MetadataParam[Real <: RealRpcSymbol](
    val owner: MetadataConstructor[Real], val symbol: Symbol) extends RpcParam {

    def shortDescription = "metadata parameter"
    def description = s"$shortDescription $nameStr of ${owner.description}"
  }

  sealed abstract class CompositeMetadataParam[Real <: RealRpcSymbol](
    owner: MetadataConstructor[Real], symbol: Symbol) extends MetadataParam[Real](owner, symbol) {
    val constructor: MetadataConstructor[Real]

    override def description: String = s"${super.description} at ${owner.description}"
  }

  class RpcCompositeParam(override val owner: RpcMetadataConstructor, symbol: Symbol)
    extends CompositeMetadataParam[RealRpcTrait](owner, symbol) {
    val constructor: RpcMetadataConstructor = new RpcMetadataConstructor(actualType, Some(this))
  }

  class MethodCompositeParam(override val owner: MethodMetadataConstructor, symbol: Symbol)
    extends CompositeMetadataParam[RealMethod](owner, symbol) {
    val constructor: MethodMetadataConstructor = new MethodMetadataConstructor(actualType, Right(this))

    def pathStr: String = owner.atParam.fold(_ => nameStr, cp => s"${cp.pathStr}.$nameStr")
  }

  class ParamCompositeParam(override val owner: ParamMetadataConstructor, symbol: Symbol)
    extends CompositeMetadataParam[RealParam](owner, symbol) {
    val constructor: ParamMetadataConstructor = new ParamMetadataConstructor(actualType, Right(this), owner.indexInRaw)
  }

  class MethodMetadataParam(owner: RpcMetadataConstructor, symbol: Symbol)
    extends MetadataParam[RealRpcTrait](owner, symbol) with RawRpcSymbol with ArityParam {

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
    extends MetadataParam[RealMethod](owner, symbol) with RealParamTarget {

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

  sealed abstract class MetadataConstructor[Real <: RealRpcSymbol](val symbol: Symbol) extends RpcMethod {
    def ownerType: Type

    def annot(tpe: Type): Option[Annot] =
      findAnnotation(symbol, tpe) orElse {
        // fallback to annotations on the class itself
        if (symbol.asMethod.isConstructor)
          findAnnotation(ownerType.typeSymbol, tpe)
        else None
      }

    def shortDescription = "metadata class"
    def description = s"$shortDescription $ownerType"

    def createDirectParam(paramSym: Symbol, annot: Annot): DirectMetadataParam[Real] = annot.tpe match {
      case t if t <:< InferAT => new ImplicitParam(this, paramSym)
      case t if t <:< ReifyAnnotAT => new ReifiedAnnotParam(this, paramSym)
      case t if t <:< ReifyNameAT =>
        val useRpcName = annot.findArg[Boolean](ReifyNameAT.member(TermName("rpcName")), false)
        new ReifiedNameParam(this, paramSym, useRpcName)
      case t if t <:< IsAnnotatedAT =>
        new IsAnnotatedParam(this, paramSym, t.typeArgs.head)
      case t => reportProblem(s"metadata param strategy $t is not allowed here")
    }

    def createCompositeParam(paramSym: Symbol): CompositeMetadataParam[Real]
    def createDefaultParam(paramSym: Symbol): MetadataParam[Real]

    lazy val paramLists: List[List[MetadataParam[Real]]] =
      symbol.typeSignatureIn(ownerType).paramLists.map(_.map { ps =>
        if (findAnnotation(ps, CompositeAT).nonEmpty)
          createCompositeParam(ps)
        else findAnnotation(ps, MetadataParamStrategyType).map(createDirectParam(ps, _))
          .getOrElse(if (ps.isImplicit) new ImplicitParam(this, ps) else createDefaultParam(ps))
      })

    def constructorCall(argDecls: List[Tree]): Tree =
      q"""
        ..$argDecls
        new $ownerType(...$argLists)
      """
  }

  case class MethodMetadataMapping(matchedMethod: MatchedMethod, mdParam: MethodMetadataParam, tree: Tree)

  class RpcMetadataConstructor(val ownerType: Type, val atParam: Option[RpcCompositeParam])
    extends MetadataConstructor[RealRpcTrait](primaryConstructor(ownerType, atParam)) with RawRpcSymbol {

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
      case rcp: RpcCompositeParam => rcp.constructor.methodMdParams
      case _ => Nil
    }

    def createDefaultParam(paramSym: Symbol): MethodMetadataParam =
      new MethodMetadataParam(this, paramSym)

    def createCompositeParam(paramSym: Symbol): RpcCompositeParam =
      new RpcCompositeParam(this, paramSym)

    def methodMappings(rpc: RealRpcTrait): Map[MethodMetadataParam, List[MethodMetadataMapping]] =
      collectMethodMappings(methodMdParams, "metadata parameters", rpc.realMethods)(_.mappingFor(_)).groupBy(_.mdParam)

    def materializeFor(rpc: RealRpcTrait, methodMappings: Map[MethodMetadataParam, List[MethodMetadataMapping]]): Tree = {
      val argDecls = paramLists.flatten.map {
        case rcp: RpcCompositeParam =>
          rcp.localValueDecl(rcp.constructor.materializeFor(rpc, methodMappings))
        case dmp: DirectMetadataParam[RealRpcTrait] =>
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
      }
      constructorCall(argDecls)
    }
  }

  class MethodMetadataConstructor(
    val ownerType: Type,
    val atParam: Either[MethodMetadataParam, MethodCompositeParam]
  ) extends MetadataConstructor[RealMethod](
    primaryConstructor(ownerType, Some(atParam.fold[RpcSymbol](identity, identity)))) {

    val containingMethodParam: MethodMetadataParam =
      atParam.fold(identity, _.owner.containingMethodParam)

    lazy val paramMdParams: List[ParamMetadataParam] = paramLists.flatten.flatMap {
      case pmp: ParamMetadataParam => List(pmp)
      case mcp: MethodCompositeParam => mcp.constructor.paramMdParams
      case _ => Nil
    }

    def createDefaultParam(paramSym: Symbol): ParamMetadataParam =
      new ParamMetadataParam(this, paramSym)

    def createCompositeParam(paramSym: Symbol): MethodCompositeParam =
      new MethodCompositeParam(this, paramSym)

    def paramMappings(matchedMethod: MatchedMethod): Res[Map[ParamMetadataParam, Tree]] =
      collectParamMappings(paramMdParams, "metadata parameter", matchedMethod)(
        (param, parser) => param.metadataFor(parser).map(t => (param, t))).map(_.toMap)

    def tryMaterializeFor(matchedMethod: MatchedMethod, paramMappings: Map[ParamMetadataParam, Tree]): Res[Tree] =
      Res.traverse(paramLists.flatten) {
        case cmp: MethodCompositeParam =>
          cmp.constructor.tryMaterializeFor(matchedMethod, paramMappings).map(cmp.localValueDecl)
        case dmp: DirectMetadataParam[RealMethod] =>
          dmp.tryMaterializeFor(matchedMethod).map(dmp.localValueDecl)
        case pmp: ParamMetadataParam =>
          Ok(pmp.localValueDecl(paramMappings(pmp)))
      }.map(constructorCall)
  }

  class ParamMetadataConstructor(
    val ownerType: Type,
    val atParam: Either[ParamMetadataParam, ParamCompositeParam],
    val indexInRaw: Int
  ) extends MetadataConstructor[RealParam](
    primaryConstructor(ownerType, Some(atParam.fold[RpcSymbol](identity, identity)))) {

    override def createDirectParam(paramSym: Symbol, annot: Annot): DirectMetadataParam[RealParam] =
      annot.tpe match {
        case t if t <:< ReifyPositionAT => new ReifiedPositionParam(this, paramSym)
        case t if t <:< ReifyFlagsAT => new ReifiedFlagsParam(this, paramSym)
        case _ => super.createDirectParam(paramSym, annot)
      }

    def createDefaultParam(paramSym: Symbol): UnknownParam[RealParam] =
      new UnknownParam(this, paramSym)

    def createCompositeParam(paramSym: Symbol): ParamCompositeParam =
      new ParamCompositeParam(this, paramSym)

    def tryMaterializeFor(matchedParam: MatchedParam): Res[Tree] =
      Res.traverse(paramLists.flatten) {
        case pcp: ParamCompositeParam =>
          pcp.constructor.tryMaterializeFor(matchedParam).map(pcp.localValueDecl)
        case dmp: DirectMetadataParam[RealParam] =>
          dmp.tryMaterializeFor(matchedParam).map(dmp.localValueDecl)
      }.map(constructorCall)
  }

  sealed abstract class DirectMetadataParam[Real <: RealRpcSymbol](owner: MetadataConstructor[Real], symbol: Symbol)
    extends MetadataParam[Real](owner, symbol) {

    def materializeFor(matchedSymbol: MatchedRealSymbol[Real]): Tree
    def tryMaterializeFor(matchedSymbol: MatchedRealSymbol[Real]): Res[Tree]
  }

  class ImplicitParam[Real <: RealRpcSymbol](owner: MetadataConstructor[Real], symbol: Symbol)
    extends DirectMetadataParam[Real](owner, symbol) {

    val checked: Boolean = findAnnotation(symbol, CheckedAT).nonEmpty

    def materializeFor(matchedSymbol: MatchedRealSymbol[Real]): Tree =
      q"${infer(actualType)}"

    def tryMaterializeFor(matchedSymbol: MatchedRealSymbol[Real]): Res[Tree] =
      if (checked)
        tryInferCachedImplicit(actualType).map(n => Ok(q"$n"))
          .getOrElse(Fail(s"no implicit value $actualType for $description could be found"))
      else
        Ok(materializeFor(matchedSymbol))
  }

  class ReifiedAnnotParam[Real <: RealRpcSymbol](owner: MetadataConstructor[Real], symbol: Symbol)
    extends DirectMetadataParam[Real](owner, symbol) with ArityParam {

    def allowMulti: Boolean = true
    def allowNamedMulti: Boolean = false
    def allowListedMulti: Boolean = true

    if (!(arity.collectedType <:< typeOf[StaticAnnotation])) {
      reportProblem(s"${arity.collectedType} is not a subtype of StaticAnnotation")
    }

    def materializeFor(matchedSymbol: MatchedRealSymbol[Real]): Tree = {
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

    def tryMaterializeFor(matchedSymbol: MatchedRealSymbol[Real]): Res[Tree] =
      Ok(materializeFor(matchedSymbol))
  }

  class IsAnnotatedParam[Real <: RealRpcSymbol](owner: MetadataConstructor[Real], symbol: Symbol, annotTpe: Type)
    extends DirectMetadataParam[Real](owner, symbol) {

    if (!(actualType =:= typeOf[Boolean])) {
      reportProblem("@hasAnnot can only be used on Boolean parameters")
    }

    def materializeFor(matchedSymbol: MatchedRealSymbol[Real]): Tree =
      q"${matchedSymbol.allAnnots(annotTpe).nonEmpty}"
    def tryMaterializeFor(matchedSymbol: MatchedRealSymbol[Real]): Res[Tree] =
      Ok(materializeFor(matchedSymbol))
  }

  class ReifiedNameParam[Real <: RealRpcSymbol](owner: MetadataConstructor[Real], symbol: Symbol, useRpcName: Boolean)
    extends DirectMetadataParam[Real](owner, symbol) {

    if (!(actualType =:= typeOf[String])) {
      reportProblem(s"its type is not String")
    }

    def materializeFor(matchedSymbol: MatchedRealSymbol[Real]): Tree =
      q"${if (useRpcName) matchedSymbol.rpcName else matchedSymbol.real.nameStr}"

    def tryMaterializeFor(matchedSymbol: MatchedRealSymbol[Real]): Res[Tree] =
      Ok(materializeFor(matchedSymbol))
  }

  class ReifiedPositionParam(owner: ParamMetadataConstructor, symbol: Symbol)
    extends DirectMetadataParam[RealParam](owner, symbol) {

    if (!(actualType =:= ParamPositionTpe)) {
      reportProblem("its type is not ParamPosition")
    }

    def materializeFor(matchedParam: MatchedRealSymbol[RealParam]): Tree = {
      val rpcSym = matchedParam.real
      q"$ParamPositionObj(${rpcSym.index}, ${rpcSym.indexOfList}, ${rpcSym.indexInList}, ${owner.indexInRaw})"
    }

    def tryMaterializeFor(matchedParam: MatchedRealSymbol[RealParam]): Res[Tree] =
      Ok(materializeFor(matchedParam))
  }

  class ReifiedFlagsParam(owner: ParamMetadataConstructor, symbol: Symbol)
    extends DirectMetadataParam[RealParam](owner, symbol) {

    if (!(actualType =:= ParamFlagsTpe)) {
      reportProblem("its type is not ParamFlags")
    }

    def materializeFor(matchedParam: MatchedRealSymbol[RealParam]): Tree = {
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

    def tryMaterializeFor(matchedParam: MatchedRealSymbol[RealParam]): Res[Tree] =
      Ok(materializeFor(matchedParam))
  }

  class UnknownParam[Real <: RealRpcSymbol](owner: MetadataConstructor[Real], symbol: Symbol)
    extends DirectMetadataParam[Real](owner, symbol) {

    def materializeFor(matchedSymbol: MatchedRealSymbol[Real]): Tree =
      reportProblem(s"no strategy annotation (e.g. @infer) found")
    def tryMaterializeFor(matchedSymbol: MatchedRealSymbol[Real]): Res[Tree] =
      Ok(materializeFor(matchedSymbol))
  }
}
