package com.avsystem.commons
package macros.rpc

import com.avsystem.commons.macros.meta.MacroMetadatas
import com.avsystem.commons.macros.misc.{FailMsg, Ok, Res}

private[commons] trait RpcMetadatas extends MacroMetadatas { this: RpcMacroCommons with RpcSymbols with RpcMappings =>

  import c.universe._

  class MethodMetadataParam(owner: MetadataConstructor, symbol: Symbol)
    extends MetadataParam(owner, symbol) with RealMethodTarget with ArityParam {

    def allowNamedMulti: Boolean = true
    def allowListedMulti: Boolean = true
    def allowFail: Boolean = false

    def baseTagSpecs: List[BaseTagSpec] = tagSpecs(MethodTagAT)

    if (!(typeGivenInstances <:< TypedMetadataType)) {
      reportProblem(s"method metadata type must be a subtype TypedMetadata[_]")
    }

    def mappingFor(matchedMethod: MatchedMethod): Res[MethodMetadataMapping] = for {
      mdType <- actualMetadataType(typeGivenInstances, matchedMethod.real.resultType, "method result type", verbatimResult)
      tree <- materializeOneOf(mdType) { t =>
        val constructor = new MethodMetadataConstructor(t, this, this)
        for {
          newMatchedMethod <- constructor.matchTagsAndFilters(matchedMethod)
          paramMappings <- constructor.paramMappings(newMatchedMethod)
          typeParamMappings <- constructor.typeParamMappings(newMatchedMethod)
          tree <- constructor.tryMaterializeFor(newMatchedMethod, paramMappings, typeParamMappings)
        } yield withTypeParamInstances(matchedMethod.typeParamsInContext)(tree)
      }
    } yield MethodMetadataMapping(matchedMethod, this, tree)

    def allowImplicitDepParams: Boolean = false
  }

  class TypeParamMetadataParam(owner: MetadataConstructor, symbol: Symbol)
    extends MetadataParam(owner, symbol) with RealTypeParamTarget {

    def baseTagSpecs: List[BaseTagSpec] = Nil //TODO: introduce `typeParamTag` just for the sake of consistency?

    private def metadataTree(matchedMethod: MatchedMethod, realParam: RealTypeParam, indexInRaw: Int): Option[Res[Tree]] =
      matchRealTypeParam(matchedMethod, realParam, indexInRaw).toOption.map(metadataTree(_, indexInRaw))

    private def metadataTree(matchedParam: MatchedTypeParam, indexInRaw: Int): Res[Tree] = {
      val realParam = matchedParam.real
      val result = for {
        tree <- materializeOneOf(arity.collectedType) { t =>
          val constructor = new TypeParamMetadataConstructor(t, this, this, indexInRaw)
          for {
            newMatchedParam <- constructor.matchTagsAndFilters(matchedParam)
            tree <- constructor.tryMaterializeFor(newMatchedParam)
          } yield tree
        }
      } yield tree
      result.mapFailure(msg => s"${realParam.problemStr}:\n$msg")
    }

    def metadataFor(matchedMethod: MatchedMethod, parser: ParamsParser[RealTypeParam]): Res[Tree] = arity match {
      case _: ParamArity.Single =>
        val errorMessage = unmatchedError.getOrElse(s"$shortDescription $pathStr was not matched by any real type parameter")
        parser.extractSingle(!auxiliary, metadataTree(matchedMethod, _, 0), errorMessage)
      case _: ParamArity.Optional =>
        Ok(mkOptional(parser.extractOptional(!auxiliary, metadataTree(matchedMethod, _, 0))))
      case ParamArity.Multi(_, true) =>
        parser.extractMulti(!auxiliary, (rp, i) => matchRealTypeParam(matchedMethod, rp, i)
          .toOption.map(mp => metadataTree(mp, i).map(t => q"(${mp.rawName}, $t)"))).map(mkMulti(_))
      case _: ParamArity.Multi =>
        parser.extractMulti(!auxiliary, metadataTree(matchedMethod, _, _)).map(mkMulti(_))
    }
  }

  class ParamMetadataParam(owner: MetadataConstructor, symbol: Symbol)
    extends MetadataParam(owner, symbol) with RealParamTarget {

    def baseTagSpecs: List[BaseTagSpec] = tagSpecs(ParamTagAT)

    if (!(typeGivenInstances <:< TypedMetadataType)) {
      reportProblem(s"type $typeGivenInstances is not a subtype of TypedMetadata[_]")
    }

    private def metadataTree(matchedMethod: MatchedMethod, realParam: RealParam, indexInRaw: Int): Option[Res[Tree]] =
      matchRealParam(matchedMethod, realParam, indexInRaw).toOption.map(metadataTree(_, indexInRaw))

    private def metadataTree(matchedParam: MatchedParam, indexInRaw: Int): Res[Tree] = {
      val realParam = matchedParam.real
      val result = for {
        mdType <- actualMetadataType(typeGivenInstances, realParam.actualType, "parameter type", verbatim)
        tree <- materializeOneOf(mdType) { t =>
          val constructor = new ParamMetadataConstructor(t, this, this, indexInRaw)
          for {
            newMatchedParam <- constructor.matchTagsAndFilters(matchedParam)
            tree <- constructor.tryMaterializeFor(newMatchedParam)
          } yield tree
        }
      } yield tree
      result.mapFailure(msg => s"${realParam.problemStr}:\n$msg")
    }

    def metadataFor(matchedMethod: MatchedMethod, parser: ParamsParser[RealParam]): Res[Tree] = {
      val res = arity match {
        case _: ParamArity.Single =>
          val errorMessage = unmatchedError.getOrElse(s"$shortDescription $pathStr was not matched by any real parameter")
          parser.extractSingle(!auxiliary, metadataTree(matchedMethod, _, 0), errorMessage)
        case _: ParamArity.Optional =>
          Ok(mkOptional(parser.extractOptional(!auxiliary, metadataTree(matchedMethod, _, 0))))
        case ParamArity.Multi(_, true) =>
          parser.extractMulti(!auxiliary, (rp, i) => matchRealParam(matchedMethod, rp, i)
            .toOption.map(mp => metadataTree(mp, i).map(t => q"(${mp.rawName}, $t)"))).map(mkMulti(_))
        case _: ParamArity.Multi =>
          parser.extractMulti(!auxiliary, metadataTree(matchedMethod, _, _)).map(mkMulti(_))
      }
      res.map(withTypeParamInstances(matchedMethod.typeParamsInContext))
    }
  }

  case class MethodMetadataMapping(matchedMethod: MatchedMethod, mdParam: MethodMetadataParam, tree: Tree) {
    def collectedTree(named: Boolean): Tree = if (named) q"(${matchedMethod.rawName}, $tree)" else tree
  }

  class RpcTraitMetadataConstructor(constructed: Type, ownerParam: Option[MetadataParam])
    extends MetadataConstructor(constructed, ownerParam) with TagMatchingSymbol {

    def baseTagSpecs: List[BaseTagSpec] = Nil

    lazy val methodMdParams: List[MethodMetadataParam] = collectParams[MethodMetadataParam]

    override def paramByStrategy(paramSym: Symbol, annot: Annot, ownerConstr: MetadataConstructor): MetadataParam =
      if (annot.tpe <:< RpcMethodMetadataAT) new MethodMetadataParam(ownerConstr, paramSym)
      else super.paramByStrategy(paramSym, annot, ownerConstr)

    def compositeConstructor(param: CompositeParam): MetadataConstructor =
      new RpcTraitMetadataConstructor(param.collectedType, Some(param))

    def tryMaterializeFor(rpc: RealRpcApi): Res[Tree] = {
      val errorBase = unmatchedError.getOrElse(s"cannot materialize ${constructed.typeSymbol} for $rpc")
      val methodMappings =
        collectMethodMappings(
          methodMdParams, errorBase, rpc.realMethods, allowIncomplete
        )(_.mappingFor(_)).groupBy(_.mdParam)

      tryMaterialize(rpc) { case mmp: MethodMetadataParam =>
        val mappings = methodMappings.getOrElse(mmp, Nil)
        mmp.arity match {
          case ParamArity.Single(_) => mappings match {
            case Nil => FailMsg(s"no real method found that would match ${mmp.description}")
            case List(m) => Ok(m.tree)
            case _ => FailMsg(s"multiple real methods match ${mmp.description}")
          }
          case ParamArity.Optional(_) => mappings match {
            case Nil => Ok(mmp.mkOptional[Tree](None))
            case List(m) => Ok(mmp.mkOptional(Some(m.tree)))
            case _ => FailMsg(s"multiple real methods match ${mmp.description}")
          }
          case ParamArity.Multi(_, named) =>
            Ok(mmp.mkMulti(mappings.map(_.collectedTree(named))))
          case arity =>
            FailMsg(s"${arity.annotStr} not allowed on method metadata params")
        }
      }
    }
  }

  class MethodMetadataConstructor(
    constructed: Type,
    val containingMethodParam: MethodMetadataParam,
    owner: MetadataParam
  ) extends MetadataConstructor(constructed, Some(owner)) {

    def baseTagSpecs: List[BaseTagSpec] = tagSpecs(MethodTagAT)

    lazy val paramMdParams: List[ParamMetadataParam] = collectParams[ParamMetadataParam]
    lazy val typeParamMdParams: List[TypeParamMetadataParam] = collectParams[TypeParamMetadataParam]

    override def paramByStrategy(paramSym: Symbol, annot: Annot, ownerConstr: MetadataConstructor = this): MetadataParam =
      if (annot.tpe <:< ReifyParamListCountAT) new ReifiedParamListCountParam(ownerConstr, paramSym)
      else if (annot.tpe <:< RpcParamMetadataAT) new ParamMetadataParam(ownerConstr, paramSym)
      else if (annot.tpe <:< RpcTypeParamMetadataAT) new TypeParamMetadataParam(ownerConstr, paramSym)
      else super.paramByStrategy(paramSym, annot, ownerConstr)

    def compositeConstructor(param: CompositeParam): MetadataConstructor =
      new MethodMetadataConstructor(param.collectedType, containingMethodParam, param)

    def paramMappings(matchedMethod: MatchedMethod): Res[Map[ParamMetadataParam, Tree]] =
      collectParamMappings(matchedMethod.real.realParams, paramMdParams, allowIncomplete)(
        (param, parser) => param.metadataFor(matchedMethod, parser).map(t => (param, t)),
        rp => containingMethodParam.errorForUnmatchedParam(rp).getOrElse(
          s"no metadata parameter was found that would match ${rp.shortDescription} ${rp.nameStr}")
      ).map(_.toMap)

    def typeParamMappings(matchedMethod: MatchedMethod): Res[Map[TypeParamMetadataParam, Tree]] =
      collectParamMappings(matchedMethod.real.typeParams, typeParamMdParams, allowIncomplete)(
        (param, parser) => param.metadataFor(matchedMethod, parser).map(t => (param, t)),
        rp => containingMethodParam.errorForUnmatchedParam(rp).getOrElse(
          s"no metadata parameter was found that would match ${rp.shortDescription} ${rp.nameStr}")
      ).map(_.toMap)

    def tryMaterializeFor(
      matchedMethod: MatchedMethod,
      paramMappings: Map[ParamMetadataParam, Tree],
      typeParamMappings: Map[TypeParamMetadataParam, Tree]
    ): Res[Tree] =
      tryMaterialize(matchedMethod) {
        case pmp: ParamMetadataParam => Ok(paramMappings(pmp))
        case tpmp: TypeParamMetadataParam => Ok(typeParamMappings(tpmp))
      }
  }

  class TypeParamMetadataConstructor(
    constructed: Type,
    containingTypeParamMdParam: TypeParamMetadataParam,
    owner: MetadataParam,
    val indexInRaw: Int
  ) extends MetadataConstructor(constructed, Some(owner)) {
    def baseTagSpecs: List[BaseTagSpec] = Nil

    override def paramByStrategy(paramSym: Symbol, annot: Annot, ownerConstr: MetadataConstructor): MetadataParam =
      annot.tpe match {
        // TODO: metadata for lower/upper bound or something?
        case _ => super.paramByStrategy(paramSym, annot, ownerConstr)
      }

    def compositeConstructor(param: CompositeParam): MetadataConstructor =
      new TypeParamMetadataConstructor(param.collectedType, containingTypeParamMdParam, param, indexInRaw)

    def tryMaterializeFor(matchedTypeParam: MatchedTypeParam): Res[Tree] =
      tryMaterialize(matchedTypeParam)(p => FailMsg(s"unexpected metadata parameter $p"))
  }

  class ParamMetadataConstructor(
    constructed: Type,
    containingParamMdParam: ParamMetadataParam,
    owner: MetadataParam,
    val indexInRaw: Int
  ) extends MetadataConstructor(constructed, Some(owner)) {

    def baseTagSpecs: List[BaseTagSpec] = tagSpecs(ParamTagAT)

    override def paramByStrategy(paramSym: Symbol, annot: Annot, ownerConstr: MetadataConstructor): MetadataParam =
      annot.tpe match {
        case t if t <:< ReifyPositionAT => new ParamPositionParam(ownerConstr, paramSym)
        case t if t <:< ReifyFlagsAT => new ParamFlagsParam(ownerConstr, paramSym)
        case _ => super.paramByStrategy(paramSym, annot, ownerConstr)
      }

    def compositeConstructor(param: CompositeParam): MetadataConstructor =
      new ParamMetadataConstructor(param.typeGivenInstances, containingParamMdParam, param, indexInRaw)

    def tryMaterializeFor(matchedParam: MatchedParam): Res[Tree] =
      tryMaterialize(matchedParam)(p => FailMsg(s"unexpected metadata parameter $p"))
  }

  class ReifiedParamListCountParam(owner: MetadataConstructor, symbol: Symbol)
    extends DirectMetadataParam(owner, symbol) {

    if (!(actualType =:= definitions.IntTpe)) {
      reportProblem("its type is not Int")
    }

    def tryMaterializeFor(matchedSymbol: MatchedSymbol): Res[Tree] =
      Ok(q"${matchedSymbol.real.symbol.asMethod.paramLists.length}")
  }
}
