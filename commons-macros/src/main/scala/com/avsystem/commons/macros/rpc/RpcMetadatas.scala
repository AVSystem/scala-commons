package com.avsystem.commons
package macros.rpc

import com.avsystem.commons.macros.meta.MacroMetadatas
import com.avsystem.commons.macros.misc.{FailMsg, Ok, Res}

private[commons] trait RpcMetadatas extends MacroMetadatas { this: RpcMacroCommons with RpcSymbols with RpcMappings =>

  import c.universe._

  class MethodMetadataParam(owner: RpcTraitMetadataConstructor, symbol: Symbol)
    extends MetadataParam(owner, symbol) with RealMethodTarget with ArityParam {

    def allowNamedMulti: Boolean = true
    def allowListedMulti: Boolean = true
    def allowFail: Boolean = false

    def baseTagSpecs: List[BaseTagSpec] = tagSpecs(MethodTagAT)

    if (!(arity.collectedType <:< TypedMetadataType)) {
      reportProblem(s"method metadata type must be a subtype TypedMetadata[_]")
    }

    def mappingFor(matchedMethod: MatchedMethod): Res[MethodMetadataMapping] = for {
      mdType <- actualMetadataType(arity.collectedType, matchedMethod.real.resultType, "method result type", verbatimResult)
      tree <- materializeOneOf(mdType) { t =>
        val constructor = new MethodMetadataConstructor(t, this, this)
        for {
          newMatchedMethod <- constructor.matchTagsAndFilters(matchedMethod)
          paramMappings <- constructor.paramMappings(newMatchedMethod)
          tree <- constructor.tryMaterializeFor(newMatchedMethod, paramMappings)
        } yield tree
      }
    } yield MethodMetadataMapping(matchedMethod, this, tree)
  }

  class ParamMetadataParam(owner: MethodMetadataConstructor, symbol: Symbol)
    extends MetadataParam(owner, symbol) with RealParamTarget {

    def baseTagSpecs: List[BaseTagSpec] = tagSpecs(ParamTagAT)

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
          val constructor = new ParamMetadataConstructor(t, this, this, indexInRaw)
          for {
            newMatchedParam <- constructor.matchTagsAndFilters(matchedParam)
            tree <- constructor.tryMaterializeFor(newMatchedParam)
          } yield tree
        }
      } yield tree
      result.mapFailure(msg => s"${realParam.problemStr}:\n$msg")
    }

    def metadataFor(matchedMethod: MatchedMethod, parser: ParamsParser[RealParam]): Res[Tree] = arity match {
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
  }

  case class MethodMetadataMapping(matchedMethod: MatchedMethod, mdParam: MethodMetadataParam, tree: Tree) {
    def collectedTree(named: Boolean): Tree = if (named) q"(${matchedMethod.rawName}, $tree)" else tree
  }

  class RpcTraitMetadataConstructor(constructed: Type, ownerParam: Option[MetadataParam])
    extends MetadataConstructor(constructed, ownerParam) with TagMatchingSymbol {

    def baseTagSpecs: List[BaseTagSpec] = Nil

    lazy val methodMdParams: List[MethodMetadataParam] = collectParams[MethodMetadataParam]

    override def paramByStrategy(paramSym: Symbol, annot: Annot): MetadataParam =
      if (annot.tpe <:< RpcMethodMetadataAT) new MethodMetadataParam(this, paramSym)
      else super.paramByStrategy(paramSym, annot)

    def compositeConstructor(param: CompositeParam): MetadataConstructor =
      new RpcTraitMetadataConstructor(param.collectedType, Some(param))

    def tryMaterializeFor(rpc: RealRpcTrait): Res[Tree] = {
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

    override def paramByStrategy(paramSym: Symbol, annot: Annot): MetadataParam =
      if (annot.tpe <:< RpcParamMetadataAT) new ParamMetadataParam(this, paramSym)
      else super.paramByStrategy(paramSym, annot)

    def compositeConstructor(param: CompositeParam): MetadataConstructor =
      new MethodMetadataConstructor(param.collectedType, containingMethodParam, param)

    def paramMappings(matchedMethod: MatchedMethod): Res[Map[ParamMetadataParam, Tree]] =
      collectParamMappings(matchedMethod.real.realParams, paramMdParams, allowIncomplete)(
        (param, parser) => param.metadataFor(matchedMethod, parser).map(t => (param, t)),
        rp => containingMethodParam.errorForUnmatchedParam(rp).getOrElse(
          s"no metadata parameter was found that would match ${rp.shortDescription} ${rp.nameStr}")
      ).map(_.toMap)

    def tryMaterializeFor(matchedMethod: MatchedMethod, paramMappings: Map[ParamMetadataParam, Tree]): Res[Tree] =
      tryMaterialize(matchedMethod) {
        case pmp: ParamMetadataParam => Ok(paramMappings(pmp))
      }
  }

  class ParamMetadataConstructor(
    constructed: Type,
    containingParamMdParam: ParamMetadataParam,
    owner: MetadataParam,
    val indexInRaw: Int
  ) extends MetadataConstructor(constructed, Some(owner)) {

    def baseTagSpecs: List[BaseTagSpec] = tagSpecs(ParamTagAT)

    override def paramByStrategy(paramSym: Symbol, annot: Annot): MetadataParam =
      annot.tpe match {
        case t if t <:< ReifyPositionAT => new ParamPositionParam(this, paramSym)
        case t if t <:< ReifyFlagsAT => new ParamFlagsParam(this, paramSym)
        case _ => super.paramByStrategy(paramSym, annot)
      }

    def compositeConstructor(param: CompositeParam): MetadataConstructor =
      new ParamMetadataConstructor(param.collectedType, containingParamMdParam, param, indexInRaw)

    def tryMaterializeFor(matchedParam: MatchedParam): Res[Tree] =
      tryMaterialize(matchedParam)(p => FailMsg(s"unexpected metadata parameter $p"))
  }
}
