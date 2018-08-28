package com.avsystem.commons
package macros.rpc

import com.avsystem.commons.macros.misc.MacroMetadatas

trait RpcMetadatas extends MacroMetadatas { this: RpcMacroCommons with RpcSymbols with RpcMappings =>

  import c.universe._

  class MethodMetadataParam(owner: RpcTraitMetadataConstructor, symbol: Symbol)
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
      mdType <- actualMetadataType(arity.collectedType, matchedMethod.real.resultType, "method result type", verbatimResult)
      constructor = new MethodMetadataConstructor(mdType, Left(this))
      paramMappings <- constructor.paramMappings(matchedMethod)
      tree <- constructor.tryMaterializeFor(matchedMethod, paramMappings)
    } yield MethodMetadataMapping(matchedMethod, this, tree)
  }

  class ParamMetadataParam(owner: MethodMetadataConstructor, symbol: Symbol)
    extends MetadataParam(owner, symbol) with RealParamTarget {

    def pathStr: String = {
      def cpPath(cp: CompositeParam): String =
        cp.owner.forMethod.atParam.fold(_ => nameStr, cp => s"${cpPath(cp)}.$nameStr")
      owner.atParam.fold(_ => nameStr, cp => s"${cpPath(cp)}.$nameStr")
    }

    def baseTagTpe: Type = owner.containingMethodParam.baseParamTag
    def fallbackTag: FallbackTag = owner.containingMethodParam.fallbackParamTag

    def cannotMapClue = s"cannot map it to $shortDescription $nameStr of ${owner.ownerType}"

    if (!(arity.collectedType <:< TypedMetadataType)) {
      reportProblem(s"type ${arity.collectedType} is not a subtype of TypedMetadata[_]")
    }

    private def metadataTree(matchedMethod: MatchedMethod, realParam: RealParam, indexInRaw: Int): Option[Res[Tree]] =
      matchRealParam(matchedMethod, realParam, indexInRaw).toOption.map(metadataTree(_, indexInRaw))

    private def metadataTree(matchedParam: MatchedParam, indexInRaw: Int): Res[Tree] = {
      val realParam = matchedParam.real
      val result = for {
        mdType <- actualMetadataType(arity.collectedType, realParam.actualType, "parameter type", verbatim)
        tree <- new ParamMetadataConstructor(mdType, Left(this), indexInRaw).tryMaterializeFor(matchedParam)
      } yield tree
      result.mapFailure(msg => s"${realParam.problemStr}: $cannotMapClue: $msg")
    }

    def metadataFor(matchedMethod: MatchedMethod, parser: ParamsParser[RealParam]): Res[Tree] = arity match {
      case _: ParamArity.Single =>
        val unmatchedError = s"$shortDescription $pathStr was not matched by real parameter"
        parser.extractSingle(!auxiliary, metadataTree(matchedMethod, _, 0), unmatchedError)
      case _: ParamArity.Optional =>
        Ok(mkOptional(parser.extractOptional(!auxiliary, metadataTree(matchedMethod, _, 0))))
      case ParamArity.Multi(_, true) =>
        parser.extractMulti(!auxiliary, (rp, i) => matchRealParam(matchedMethod, rp, i)
          .toOption.map(mp => metadataTree(mp, i).map(t => q"(${mp.rpcName}, $t)"))).map(mkMulti(_))
      case _: ParamArity.Multi =>
        parser.extractMulti(!auxiliary, metadataTree(matchedMethod, _, _)).map(mkMulti(_))
    }
  }

  sealed abstract class MetadataConstructor(constructor: Symbol) extends BaseMetadataConstructor(constructor) {
    def forRpc: RpcTraitMetadataConstructor = cast[RpcTraitMetadataConstructor]
    def forMethod: MethodMetadataConstructor = cast[MethodMetadataConstructor]
    def forParam: ParamMetadataConstructor = cast[ParamMetadataConstructor]
  }

  case class MethodMetadataMapping(matchedMethod: MatchedMethod, mdParam: MethodMetadataParam, tree: Tree)

  class RpcTraitMetadataConstructor(val ownerType: Type, val atParam: Option[CompositeParam])
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
      case cp: CompositeParam => cp.constructor.forRpc.methodMdParams
      case _ => Nil
    }

    override def paramByStrategy(paramSym: Symbol, annot: Annot): MetadataParam =
      if (annot.tpe <:< RpcMethodMetadataAT) new MethodMetadataParam(this, paramSym)
      else super.paramByStrategy(paramSym, annot)

    def compositeConstructor(param: CompositeParam): MetadataConstructor =
      new RpcTraitMetadataConstructor(param.actualType, Some(param))

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
            case ParamArity.Single(_) => mappings match {
              case Nil => abort(s"no real method found that would match ${mmp.description}")
              case List(m) => m.tree
              case _ => abort(s"multiple real methods match ${mmp.description}")
            }
            case ParamArity.Optional(_) => mappings match {
              case Nil => mmp.mkOptional[Tree](None)
              case List(m) => mmp.mkOptional(Some(m.tree))
              case _ => abort(s"multiple real methods match ${mmp.description}")
            }
            case ParamArity.Multi(_, _) =>
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
    primaryConstructor(ownerType, Some(atParam.fold[MacroSymbol](identity, identity)))) {

    val containingMethodParam: MethodMetadataParam =
      atParam.fold(identity, _.owner.forMethod.containingMethodParam)

    lazy val paramMdParams: List[ParamMetadataParam] = paramLists.flatten.flatMap {
      case pmp: ParamMetadataParam => List(pmp)
      case cp: CompositeParam => cp.constructor.forMethod.paramMdParams
      case _ => Nil
    }

    override def paramByStrategy(paramSym: Symbol, annot: Annot): MetadataParam =
      if (annot.tpe <:< RpcParamMetadataAT) new ParamMetadataParam(this, paramSym)
      else super.paramByStrategy(paramSym, annot)

    def compositeConstructor(param: CompositeParam): MetadataConstructor =
      new MethodMetadataConstructor(param.actualType, Right(param))

    def paramMappings(matchedMethod: MatchedMethod): Res[Map[ParamMetadataParam, Tree]] =
      collectParamMappings(matchedMethod.real.realParams, paramMdParams, "metadata parameter")(
        (param, parser) => param.metadataFor(matchedMethod, parser).map(t => (param, t))).map(_.toMap)

    def tryMaterializeFor(matchedMethod: MatchedMethod, paramMappings: Map[ParamMetadataParam, Tree]): Res[Tree] =
      Res.traverse(paramLists.flatten) {
        case cmp: CompositeParam =>
          cmp.constructor.forMethod.tryMaterializeFor(matchedMethod, paramMappings).map(cmp.localValueDecl)
        case dmp: DirectMetadataParam =>
          dmp.tryMaterializeFor(matchedMethod).map(dmp.localValueDecl)
        case pmp: ParamMetadataParam =>
          Ok(pmp.localValueDecl(paramMappings(pmp)))
      }.map(constructorCall)
  }

  class ParamMetadataConstructor(
    val ownerType: Type,
    val atParam: Either[ParamMetadataParam, CompositeParam],
    val indexInRaw: Int
  ) extends MetadataConstructor(
    primaryConstructor(ownerType, Some(atParam.fold[MacroSymbol](identity, identity)))) {

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
        case cp: CompositeParam =>
          cp.constructor.forParam.tryMaterializeFor(matchedParam).map(cp.localValueDecl)
        case dmp: DirectMetadataParam =>
          dmp.tryMaterializeFor(matchedParam).map(dmp.localValueDecl)
      }.map(constructorCall)
  }
}
