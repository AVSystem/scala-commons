package com.avsystem.commons
package macros.rpc

import com.avsystem.commons.macros.meta.MacroMetadatas
import com.avsystem.commons.macros.misc.{Fail, Ok, Res}

private[commons] trait RpcMetadatas extends MacroMetadatas { this: RpcMacroCommons with RpcSymbols with RpcMappings =>

  import c.universe._

  class MethodMetadataParam(owner: RpcTraitMetadataConstructor, symbol: Symbol)
    extends MetadataParam(owner, symbol) with TagMatchingSymbol with ArityParam {

    def allowNamedMulti: Boolean = true
    def allowListedMulti: Boolean = true
    def allowFail: Boolean = false

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
      tree <- materializeOneOf(mdType) { t =>
        val constructor = new MethodMetadataConstructor(t, this, None)
        for {
          _ <- constructor.matchFilters(matchedMethod)
          paramMappings <- constructor.paramMappings(matchedMethod)
          tree <- constructor.tryMaterializeFor(matchedMethod, paramMappings)
        } yield tree
      }
    } yield MethodMetadataMapping(matchedMethod, this, tree)
  }

  class ParamMetadataParam(owner: MethodMetadataConstructor, symbol: Symbol)
    extends MetadataParam(owner, symbol) with RealParamTarget {

    def baseTagTpe: Type = owner.containingMethodParam.baseParamTag
    def fallbackTag: FallbackTag = owner.containingMethodParam.fallbackParamTag

    def cannotMapClue: String = s"cannot map it to $shortDescription $nameStr of ${owner.ownerType}"

    if (!(arity.collectedType <:< TypedMetadataType)) {
      reportProblem(s"type ${arity.collectedType} is not a subtype of TypedMetadata[_]")
    }

    private def metadataTree(matchedMethod: MatchedMethod, realParam: RealParam, indexInRaw: Int): Option[Res[Tree]] =
      matchRealParam(matchedMethod, realParam, indexInRaw).toOption.map(metadataTree(_, indexInRaw))

    private def metadataTree(matchedParam: MatchedParam, indexInRaw: Int): Res[Tree] = {
      val realParam = matchedParam.real
      val result = for {
        mdType <- actualMetadataType(arity.collectedType, realParam.actualType, "parameter type", verbatim)
        tree <- materializeOneOf(mdType) { t =>
          val constructor = new ParamMetadataConstructor(t, None, indexInRaw)
          for {
            _ <- constructor.matchFilters(matchedParam)
            tree <- constructor.tryMaterializeFor(matchedParam)
          } yield tree
        }
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
          .toOption.map(mp => metadataTree(mp, i).map(t => q"(${mp.rawName}, $t)"))).map(mkMulti(_))
      case _: ParamArity.Multi =>
        parser.extractMulti(!auxiliary, metadataTree(matchedMethod, _, _)).map(mkMulti(_))
      case ParamArity.Fail(error) =>
        parser.findFirst(rp => matchRealParam(matchedMethod, rp, 0).toOption)
          .map(m => Fail(s"${m.real.problemStr}: $error"))
          .getOrElse(Ok(q"()"))
    }
  }

  case class MethodMetadataMapping(matchedMethod: MatchedMethod, mdParam: MethodMetadataParam, tree: Tree) {
    def collectedTree(named: Boolean): Tree = if (named) q"(${matchedMethod.rawName}, $tree)" else tree
  }

  class RpcTraitMetadataConstructor(ownerType: Type, atParam: Option[CompositeParam])
    extends MetadataConstructor(ownerType, atParam) with TagMatchingSymbol {

    def baseTagTpe: Type = NothingTpe
    def fallbackTag: FallbackTag = FallbackTag.Empty

    val (baseMethodTag, fallbackMethodTag) =
      annot(MethodTagAT).map(tagSpec).getOrElse((NothingTpe, FallbackTag.Empty))
    val (baseParamTag, fallbackParamTag) =
      annot(ParamTagAT).map(tagSpec).getOrElse((NothingTpe, FallbackTag.Empty))

    lazy val methodMdParams: List[MethodMetadataParam] = collectParams[MethodMetadataParam]

    override def paramByStrategy(paramSym: Symbol, annot: Annot): MetadataParam =
      if (annot.tpe <:< RpcMethodMetadataAT) new MethodMetadataParam(this, paramSym)
      else super.paramByStrategy(paramSym, annot)

    def compositeConstructor(param: CompositeParam): MetadataConstructor =
      new RpcTraitMetadataConstructor(param.collectedType, Some(param))

    def methodMappings(rpc: RealRpcTrait): Map[MethodMetadataParam, List[MethodMetadataMapping]] = {
      val errorBase = s"it has no matching metadata parameters in $description"
      collectMethodMappings(
        methodMdParams, errorBase, rpc.realMethods, allowIncomplete
      )(_.mappingFor(_)).groupBy(_.mdParam)
    }

    def tryMaterializeFor(
      rpc: RealRpcTrait,
      methodMappings: Map[MethodMetadataParam, List[MethodMetadataMapping]]
    ): Res[Tree] =
      tryMaterialize(MatchedRpcTrait(rpc)) { case mmp: MethodMetadataParam =>
        val mappings = methodMappings.getOrElse(mmp, Nil)
        mmp.arity match {
          case ParamArity.Single(_) => mappings match {
            case Nil => Fail(s"no real method found that would match ${mmp.description}")
            case List(m) => Ok(m.tree)
            case _ => Fail(s"multiple real methods match ${mmp.description}")
          }
          case ParamArity.Optional(_) => mappings match {
            case Nil => Ok(mmp.mkOptional[Tree](None))
            case List(m) => Ok(mmp.mkOptional(Some(m.tree)))
            case _ => Fail(s"multiple real methods match ${mmp.description}")
          }
          case ParamArity.Multi(_, named) =>
            Ok(mmp.mkMulti(mappings.map(_.collectedTree(named))))
          case arity =>
            Fail(s"${arity.annotStr} not allowed on method metadata params")
        }
      }
  }

  class MethodMetadataConstructor(
    ownerType: Type,
    val containingMethodParam: MethodMetadataParam,
    atParam: Option[CompositeParam]
  ) extends MetadataConstructor(ownerType, atParam) {

    lazy val paramMdParams: List[ParamMetadataParam] = collectParams[ParamMetadataParam]

    override def paramByStrategy(paramSym: Symbol, annot: Annot): MetadataParam =
      if (annot.tpe <:< RpcParamMetadataAT) new ParamMetadataParam(this, paramSym)
      else super.paramByStrategy(paramSym, annot)

    def compositeConstructor(param: CompositeParam): MetadataConstructor =
      new MethodMetadataConstructor(param.collectedType, containingMethodParam, Some(param))

    def paramMappings(matchedMethod: MatchedMethod): Res[Map[ParamMetadataParam, Tree]] =
      collectParamMappings(
        matchedMethod.real.realParams, paramMdParams, "metadata parameter", allowIncomplete
      ) { (param, parser) =>
        param.metadataFor(matchedMethod, parser).map(t => (param, t))
      }.map(_.toMap)

    def tryMaterializeFor(matchedMethod: MatchedMethod, paramMappings: Map[ParamMetadataParam, Tree]): Res[Tree] =
      tryMaterialize(matchedMethod) {
        case pmp: ParamMetadataParam => Ok(paramMappings(pmp))
      }
  }

  class ParamMetadataConstructor(ownerType: Type, atParam: Option[CompositeParam], val indexInRaw: Int)
    extends MetadataConstructor(ownerType, atParam) {

    override def paramByStrategy(paramSym: Symbol, annot: Annot): MetadataParam =
      annot.tpe match {
        case t if t <:< ReifyPositionAT => new ParamPositionParam(this, paramSym)
        case t if t <:< ReifyFlagsAT => new ParamFlagsParam(this, paramSym)
        case _ => super.paramByStrategy(paramSym, annot)
      }

    def compositeConstructor(param: CompositeParam): MetadataConstructor =
      new ParamMetadataConstructor(param.collectedType, Some(param), indexInRaw)

    def tryMaterializeFor(matchedParam: MatchedParam): Res[Tree] =
      tryMaterialize(matchedParam)(p => Fail(s"unexpected metadata parameter $p"))
  }
}
