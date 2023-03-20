package com.avsystem.commons
package macros.meta

import com.avsystem.commons.macros.AbstractMacroCommons
import com.avsystem.commons.macros.misc.{Fail, FailMsg, Ok, Res}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.reflect.macros.blackbox

private[commons] class AdtMetadataMacros(ctx: blackbox.Context) extends AbstractMacroCommons(ctx) with MacroMetadatas {

  import c.universe._

  final lazy val AdtParamMetadataAT: Type = staticType(tq"$MetaPackage.adtParamMetadata")
  final lazy val AdtCaseMetadataAT: Type = staticType(tq"$MetaPackage.adtCaseMetadata")
  final lazy val AdtCaseSealedParentMetadataAT: Type = staticType(tq"$MetaPackage.adtCaseSealedParentMetadata")
  final lazy val ReifyDefaultValueAT: Type = staticType(tq"$MetaPackage.reifyDefaultValue")
  final lazy val AllowUnorderedSubtypesAT: Type = staticType(tq"$MetaPackage.allowUnorderedSubtypes")

  sealed trait AdtSymbol extends MacroTypeSymbol with SelfMatchedSymbol {
    def tpe: Type
    def seenFrom: Type = tpe
    lazy val symbol: Symbol = tpe.dealias.typeSymbol

    def cases: List[AdtCase]
    def params: List[AdtParam]
    def rootHierarchy: Option[AdtHierarchy]
  }

  class AdtHierarchy(val tpe: Type, val index: Int, val rootHierarchy: Option[AdtHierarchy], val knownSubtypes: List[Type]) extends AdtSymbol {
    def shortDescription: String = "ADT hierarchy"
    def description: String = s"$shortDescription $tpe"

    lazy val cases: List[AdtCase] = knownSubtypes.iterator.zipWithIndex.map { case (st, idx) =>
      singleValueFor(st).map(sv => new AdtObject(st, idx, Some(this), sv)) orElse
        applyUnapplyFor(st).map(au => new AdtClass(st, idx, Some(this), au)) getOrElse
        new AdtOtherCase(st, idx, Some(this))
    }.toList

    def params: List[AdtParam] = Nil
  }

  sealed trait AdtCase extends AdtSymbol {
    def index: Int
    def cases: List[AdtCase] = Nil

    lazy val sealedParents: List[AdtHierarchy] =
      rootHierarchy.fold(List.empty[AdtHierarchy]) { root =>
        symbol.asClass.baseClasses.iterator
          .takeWhile(_ != root.symbol)
          .filter(bc => isSealedHierarchyRoot(bc))
          .map { baseClass =>
            val baseType = tpe.baseType(baseClass)
            //TODO: can I use <:< here instead of base class checking?
            val filteredSubtypes = root.knownSubtypes.filter(_.typeSymbol.asClass.baseClasses.contains(baseClass))
            new AdtHierarchy(baseType, 0, rootHierarchy, filteredSubtypes)
          }
          .toList
      }
  }

  class AdtClass(val tpe: Type, val index: Int, val rootHierarchy: Option[AdtHierarchy], applyUnapply: ApplyUnapply) extends AdtCase {
    def shortDescription: String = "ADT class"
    def description: String = s"$shortDescription $tpe"

    val params: List[AdtParam] =
      applyUnapply.params.zipWithIndex.map { case (sym, idx) => new AdtParam(this, sym, idx) }

    def companion: Tree =
      typedCompanionOf(tpe).getOrElse(reportProblem(s"could not reify companion for $tpe"))
  }

  class AdtObject(val tpe: Type, val index: Int, val rootHierarchy: Option[AdtHierarchy], val singleValue: Tree) extends AdtCase {
    def shortDescription: String = "ADT object"
    def description: String = s"$shortDescription $nameStr"
    def params: List[AdtParam] = Nil
  }

  class AdtOtherCase(val tpe: Type, val index: Int, val rootHierarchy: Option[AdtHierarchy]) extends AdtCase {
    def shortDescription: String = "ADT case type"
    def description: String = s"$shortDescription $nameStr"
    def params: List[AdtParam] = Nil
  }

  class AdtParam(val owner: AdtClass, val symbol: Symbol, val index: Int) extends MacroParam {
    def seenFrom: Type = owner.tpe
    def shortDescription: String = "ADT parameter"
    def description: String = s"$shortDescription $nameStr of ${owner.description}"

    lazy val optionLike: Option[CachedImplicit] =
      if (isOptionalParam(symbol, actualType)) Some(infer(tq"$OptionLikeCls[$actualType]")) else None

    lazy val nonOptionalType: Type =
      optionLike.fold(actualType)(optionLikeValueType(_, this))
  }

  case class MatchedAdtParam(
    param: AdtParam,
    mdParam: AdtParamMetadataParam,
    indexInRaw: Int,
    fallbackTagsUsed: List[FallbackTag]
  ) extends MatchedSymbol {
    type Self = MatchedAdtParam

    def optional: Boolean =
      mdParam.allowOptionalRealParams && param.optionLike.isDefined

    def nonOptionalType: Type =
      if (mdParam.allowOptionalRealParams) param.nonOptionalType
      else param.actualType

    def index: Int = param.index
    def real: AdtParam = param
    def rawName: String = param.nameStr
    def typeParamsInContext: List[MacroTypeParam] = Nil

    def addFallbackTags(fallbackTags: List[FallbackTag]): MatchedAdtParam =
      copy(fallbackTagsUsed = fallbackTagsUsed ++ fallbackTags)
  }

  case class MatchedAdtCase(
    adtCase: AdtCase,
    mdParam: AdtCaseMetadataParam,
    indexInRaw: Int,
    fallbackTagsUsed: List[FallbackTag]
  ) extends MatchedSymbol {
    type Self = MatchedAdtCase

    def real: MacroSymbol = adtCase
    def rawName: String = adtCase.nameStr
    def index: Int = adtCase.index
    def typeParamsInContext: List[MacroTypeParam] = Nil

    def addFallbackTags(fallbackTags: List[FallbackTag]): MatchedAdtCase =
      copy(fallbackTagsUsed = fallbackTagsUsed ++ fallbackTags)
  }

  case class MatchedAdtCaseParent(
    hierarchy: AdtHierarchy,
    mdParam: AdtCaseSealedParentMetadataParam,
    indexInRaw: Int,
    fallbackTagsUsed: List[FallbackTag]
  ) extends MatchedSymbol {
    type Self = MatchedAdtCaseParent

    def real: MacroSymbol = hierarchy
    def rawName: String = hierarchy.nameStr
    def index: Int = hierarchy.index
    def typeParamsInContext: List[MacroTypeParam] = Nil

    def addFallbackTags(fallbackTags: List[FallbackTag]): MatchedAdtCaseParent =
      copy(fallbackTagsUsed = fallbackTagsUsed ++ fallbackTags)
  }

  case class AdtCaseMapping(adtCase: MatchedAdtCase, tree: Tree)
  case class AdtSealedParentMapping(parent: MatchedAdtCaseParent, tree: Tree)

  class AdtMetadataConstructor(
    constructed: Type,
    containingAdtCaseMdParam: Option[AdtCaseMetadataParam],
    ownerParam: Option[MetadataParam]
  ) extends MetadataConstructor(constructed, ownerParam) {

    override def inheritFrom: Option[TagMatchingSymbol] = containingAdtCaseMdParam

    def compositeConstructor(param: CompositeParam): MetadataConstructor =
      new AdtMetadataConstructor(param.collectedType, containingAdtCaseMdParam, Some(param))

    def baseTagSpecs: List[BaseTagSpec] = tagSpecs(CaseTagAT)

    val paramMdParams: List[AdtParamMetadataParam] = collectParams[AdtParamMetadataParam]
    val caseMdParams: List[AdtCaseMetadataParam] = collectParams[AdtCaseMetadataParam]
    val caseSealedParentsMdParams: List[AdtCaseSealedParentMetadataParam] = collectParams[AdtCaseSealedParentMetadataParam]

    if (paramMdParams.nonEmpty && caseMdParams.nonEmpty) {
      reportProblem(s"having both @adtParamMetadata and @adtCaseMetadata parameters in the same class doesn't make sense")
    }

    override def paramByStrategy(paramSym: Symbol, annot: Annot, ownerConstr: MetadataConstructor): MetadataParam =
      if (annot.tpe <:< AdtParamMetadataAT) new AdtParamMetadataParam(ownerConstr, paramSym)
      else if (annot.tpe <:< AdtCaseMetadataAT) new AdtCaseMetadataParam(ownerConstr, paramSym)
      else if (annot.tpe <:< AdtCaseSealedParentMetadataAT) new AdtCaseSealedParentMetadataParam(ownerConstr, paramSym)
      else if (annot.tpe <:< ReifyFlagsAT) new TypeFlagsParam(ownerConstr, paramSym)
      else super.paramByStrategy(paramSym, annot, ownerConstr)

    def paramMappings(params: List[AdtParam]): Res[Map[AdtParamMetadataParam, Tree]] =
      if (paramMdParams.isEmpty) Ok(Map.empty)
      else collectParamMappings(params, paramMdParams, allowIncomplete)(
        (param, parser) => param.metadataFor(parser).map(t => (param, t)),
        rp => s"no ADT metadata parameter was found that would match ${rp.shortDescription} ${rp.nameStr}"
      ).map(_.toMap)

    private def collectMappings[S <: AdtSymbol, M](
      adtSyms: List[S], mdParams: List[BaseAdtMetadataParam[S, M]], symsDescr: String
    ): Res[List[M]] =
      if (mdParams.isEmpty) Ok(Nil) else {
        val errors = new ListBuffer[String]
        def addFailure(adtSym: S, message: String): Unit = {
          errors += s" * ${adtSym.description}:\n   ${indent(message, "   ")}"
        }

        val indicesInRaw = new mutable.HashMap[BaseAdtMetadataParam[S, M], Int]
        def tryMaterialize(caseParam: BaseAdtMetadataParam[S, M], adtSym: S): Res[M] = {
          val indexInRaw = indicesInRaw.getOrElse(caseParam, 0)
          val res = caseParam.tryMaterializeFor(adtSym, indexInRaw)
          if (res.isOk) {
            indicesInRaw(caseParam) = indexInRaw + 1
          }
          res
        }

        val mappings = adtSyms.flatMap { adtSym =>
          Res.firstOk(mdParams)(tryMaterialize(_, adtSym)) { errors =>
            val unmatchedReport = errors.map { case (mdParam, err) =>
              val unmatchedError = mdParam.unmatchedError.getOrElse(
                s"${mdParam.shortDescription.capitalize} ${mdParam.nameStr} did not match")
              s" * $unmatchedError:\n   ${indent(err, "   ")}"
            }.mkString("\n")
            s"it has no matching metadata parameters:\n$unmatchedReport"
          } match {
            case Ok(m) => Some(m)
            case FailMsg(err) =>
              if (!allowIncomplete) {
                addFailure(adtSym, err)
              }
              None
            case Fail =>
              None
          }
        }

        if (errors.isEmpty) Ok(mappings)
        else FailMsg(s"some $symsDescr could not be mapped to metadata parameters:\n${errors.mkString("\n")}")
      }

    def collectCaseMappings(cases: List[AdtCase]): Res[List[AdtCaseMapping]] =
      collectMappings(cases, caseMdParams, "ADT cases")

    def collectSealedParentsMappings(parents: List[AdtHierarchy]): Res[List[AdtSealedParentMapping]] =
      collectMappings(parents, caseSealedParentsMdParams, "sealed parents")

    private def tryMaterializeFor(
      sym: AdtSymbol,
      caseMappings: Map[AdtCaseMetadataParam, List[AdtCaseMapping]],
      sealedParentsMappings: Map[AdtCaseSealedParentMetadataParam, List[AdtSealedParentMapping]],
      paramMappings: Map[AdtParamMetadataParam, Tree]
    ): Res[Tree] = tryMaterialize(sym) {
      case acp: AdtCaseMetadataParam =>
        val mappings = caseMappings.getOrElse(acp, Nil)
        acp.arity match {
          case ParamArity.Single(_) => mappings match {
            case Nil => FailMsg(s"no ADT case found that would match ${acp.description}")
            case List(m) => Ok(m.tree)
            case _ => FailMsg(s"multiple ADT cases match ${acp.description}")
          }
          case ParamArity.Optional(_) => mappings match {
            case Nil => Ok(acp.mkOptional[Tree](None))
            case List(m) => Ok(acp.mkOptional(Some(m.tree)))
            case _ => FailMsg(s"multiple ADT cases match ${acp.description}")
          }
          case ParamArity.Multi(_, named) =>
            Ok(acp.mkMulti(mappings.map(m => if (named) q"(${m.adtCase.rawName}, ${m.tree})" else m.tree)))
          case arity =>
            FailMsg(s"${arity.annotStr} not allowed on ADT case metadata params")
        }
      case apmp: AdtCaseSealedParentMetadataParam =>
        val mappings = sealedParentsMappings.getOrElse(apmp, Nil)
        Ok(apmp.mkMulti(mappings.map(_.tree)))
      case app: AdtParamMetadataParam =>
        Ok(paramMappings(app))
    }

    def tryMaterializeFor(adtSymbol: AdtSymbol): Res[Tree] =
      for {
        _ <- matchTagsAndFilters(adtSymbol)
        _ <- if (paramMdParams.isEmpty) Ok(()) else adtSymbol match {
          case _: AdtClass => Ok(())
          case _ => FailMsg(s"${adtSymbol.nameStr} is not a case class or case class like type")
        }
        _ <- if (caseMdParams.isEmpty) Ok(()) else adtSymbol match {
          case _: AdtHierarchy => Ok(())
          case _ => FailMsg(s"${adtSymbol.nameStr} is not a sealed hierarchy root")
        }
        sealedParents <- if (caseSealedParentsMdParams.isEmpty) Ok(Nil) else adtSymbol match {
          case c: AdtCase => Ok(c.sealedParents)
          case _ => FailMsg(s"${adtSymbol.nameStr} is not a case class or object in a sealed hierarchy")
        }
        pmappings <- paramMappings(adtSymbol.params)
        cmappings <- collectCaseMappings(adtSymbol.cases).map(_.groupBy(_.adtCase.mdParam))
        spmappings <- collectSealedParentsMappings(sealedParents).map(_.groupBy(_.parent.mdParam))
        tree <- tryMaterializeFor(adtSymbol, cmappings, spmappings, pmappings)
      } yield tree
  }

  class AdtParamMetadataConstructor(
    constructed: Type,
    containingAdtParamMdParam: AdtParamMetadataParam,
    owner: MetadataParam
  ) extends MetadataConstructor(constructed, Some(owner)) {

    override def inheritFrom: Option[TagMatchingSymbol] = Some(containingAdtParamMdParam)

    def baseTagSpecs: List[BaseTagSpec] = tagSpecs(ParamTagAT)

    def compositeConstructor(param: CompositeParam): MetadataConstructor =
      new AdtParamMetadataConstructor(param.collectedType, containingAdtParamMdParam, param)

    override def paramByStrategy(paramSym: Symbol, annot: Annot, ownerConstr: MetadataConstructor): MetadataParam =
      annot.tpe match {
        case t if t <:< ReifyPositionAT => new ParamPositionParam(ownerConstr, paramSym)
        case t if t <:< ReifyFlagsAT => new ParamFlagsParam(ownerConstr, paramSym)
        case t if t <:< ReifyDefaultValueAT => new ReifiedDefaultValueParam(ownerConstr, paramSym)
        case _ => super.paramByStrategy(paramSym, annot, ownerConstr)
      }

    def tryMaterializeFor(matchedParam: MatchedAdtParam): Res[Tree] =
      tryMaterialize(matchedParam)(p => FailMsg(s"unexpected metadata parameter $p"))
  }

  abstract class BaseAdtMetadataParam[S <: AdtSymbol, M](owner: MetadataConstructor, symbol: Symbol)
    extends MetadataParam(owner, symbol) with ArityParam with TagMatchingSymbol {

    def tryMaterializeFor(adtSym: S, indexInRaw: Int): Res[M]
  }

  class AdtCaseMetadataParam(owner: MetadataConstructor, symbol: Symbol)
    extends BaseAdtMetadataParam[AdtCase, AdtCaseMapping](owner, symbol) {

    def baseTagSpecs: List[BaseTagSpec] = tagSpecs(CaseTagAT)

    def allowSingle: Boolean = true
    def allowOptional: Boolean = true
    def allowNamedMulti: Boolean = true
    def allowListedMulti: Boolean = true

    def tryMaterializeFor(adtCase: AdtCase, indexInRaw: Int): Res[AdtCaseMapping] = for {
      matchedCase <- matchTagsAndFilters(MatchedAdtCase(adtCase, this, indexInRaw, Nil))
      mdType <- actualMetadataType(arity.collectedType, adtCase.tpe, "data type", verbatim = false)
      tree <- materializeOneOf(mdType)(t => new AdtMetadataConstructor(t, Some(this), Some(this)).tryMaterializeFor(adtCase))
    } yield AdtCaseMapping(matchedCase, tree)
  }

  class AdtCaseSealedParentMetadataParam(owner: MetadataConstructor, symbol: Symbol)
    extends BaseAdtMetadataParam[AdtHierarchy, AdtSealedParentMapping](owner, symbol) {

    def baseTagSpecs: List[BaseTagSpec] = Nil

    def allowSingle: Boolean = false
    def allowOptional: Boolean = false
    def allowNamedMulti: Boolean = false
    def allowListedMulti: Boolean = true

    def tryMaterializeFor(parent: AdtHierarchy, indexInRaw: Int): Res[AdtSealedParentMapping] = for {
      matchedParent <- matchTagsAndFilters(MatchedAdtCaseParent(parent, this, indexInRaw, Nil))
      mdType <- actualMetadataType(arity.collectedType, parent.tpe, "data type", verbatim = false)
      tree <- materializeOneOf(mdType)(t => new AdtMetadataConstructor(t, None, Some(this)).tryMaterializeFor(parent))
    } yield AdtSealedParentMapping(matchedParent, tree)
  }

  class AdtParamMetadataParam(owner: MetadataConstructor, symbol: Symbol)
    extends MetadataParam(owner, symbol) with ArityParam with TagMatchingSymbol {

    def allowSingle: Boolean = true
    def allowOptional: Boolean = true
    def allowNamedMulti: Boolean = true
    def allowListedMulti: Boolean = true

    def baseTagSpecs: List[BaseTagSpec] = tagSpecs(ParamTagAT)

    val auxiliary: Boolean =
      annot(AuxiliaryAT).nonEmpty

    val allowOptionalRealParams: Boolean =
      annot(AllowOptionalAT).nonEmpty

    private def matchedParam(adtParam: AdtParam, indexInRaw: Int): Res[MatchedAdtParam] =
      matchTagsAndFilters(MatchedAdtParam(adtParam, this, indexInRaw, Nil))

    private def metadataTree(adtParam: AdtParam, indexInRaw: Int): Option[Res[Tree]] =
      matchedParam(adtParam, indexInRaw).toOption.map { matched =>
        val result = for {
          mdType <- actualMetadataType(arity.collectedType, matched.nonOptionalType, "parameter type", verbatim = false)
          tree <- materializeOneOf(mdType) { t =>
            val constructor = new AdtParamMetadataConstructor(t, this, this)
            for {
              newMatched <- constructor.matchTagsAndFilters(matched)
              tree <- constructor.tryMaterializeFor(newMatched)
            } yield tree
          }
        } yield tree
        result.mapFailure(msg => s"${adtParam.problemStr}:\ncannot map it to $shortDescription $pathStr: $msg")
      }

    def metadataFor(parser: ParamsParser[AdtParam]): Res[Tree] = arity match {
      case _: ParamArity.Single =>
        val unmatchedError = s"$shortDescription $pathStr was not matched by ADT parameter"
        parser.extractSingle(!auxiliary, metadataTree(_, 0), unmatchedError)
      case _: ParamArity.Optional =>
        Ok(mkOptional(parser.extractOptional(!auxiliary, metadataTree(_, 0))))
      case ParamArity.Multi(_, true) =>
        parser.extractMulti(!auxiliary, (adtp, i) =>
          metadataTree(adtp, i).map(_.map(t => q"(${adtp.nameStr}, $t)"))).map(mkMulti(_))
      case _: ParamArity.Multi =>
        parser.extractMulti(!auxiliary, metadataTree).map(mkMulti(_))
    }
  }

  class ReifiedDefaultValueParam(owner: MetadataConstructor, symbol: Symbol)
    extends DirectMetadataParam(owner, symbol) with ArityParam {

    def allowSingle: Boolean = true
    def allowOptional: Boolean = true
    def allowNamedMulti: Boolean = false
    def allowListedMulti: Boolean = false

    val adtParamType: Type =
      owner.constructed.baseType(TypedMetadataType.typeSymbol).typeArgs.head

    if (!(arity.collectedType =:= getType(tq"$CommonsPkg.meta.DefaultValue[$adtParamType]"))) {
      reportProblem(s"type of @reifyDefaultValue metadata parameter must be " +
        s"DefaultValue[$adtParamType] but got ${arity.collectedType}")
    }

    def tryMaterializeFor(matchedSymbol: MatchedSymbol): Res[Tree] = matchedSymbol match {
      case matchedAdtParam: MatchedAdtParam =>
        val adtParam = matchedAdtParam.real
        val hasDefaultValue = adtParam.symbol.asTerm.isParamWithDefault
        def defaultValue: Tree = {
          val ownerMethodName = adtParam.symbol.owner.name.encodedName.toString
          val dvMethodName = TermName(s"$ownerMethodName$$default$$${adtParam.index + 1}")
          q"new $CommonsPkg.meta.DefaultValue[${adtParam.actualType}](${adtParam.owner.companion}.$dvMethodName)"
        }
        arity match {
          case _: ParamArity.Single =>
            if (matchedAdtParam.optional)
              FailMsg("default value cannot be reified for optional parameters")
            else if (!hasDefaultValue)
              FailMsg("no default value defined")
            else
              Ok(defaultValue)
          case _: ParamArity.Optional =>
            Ok(mkOptional(if (hasDefaultValue) Some(defaultValue) else None))
          case _ =>
            FailMsg(s"${arity.annotStr} not allowed on @reifyDefaultValue params")
        }
      case _ =>
        reportProblem("@reifyDefaultValue is allowed only for case class parameter metadata")
    }
  }

  class TypeFlagsParam(owner: MetadataConstructor, symbol: Symbol)
    extends DirectMetadataParam(owner, symbol) {
    if (!(actualType =:= TypeFlagsTpe)) {
      reportProblem("its type is not TypeFlags")
    }

    def tryMaterializeFor(matchedSymbol: MatchedSymbol): Res[Tree] = Ok {
      def flag(cond: Boolean, bit: Int) = if (cond) 1 << bit else 0
      val s = matchedSymbol.real.symbol.asType
      val rawFlags =
        flag(s.isAbstract, 0) |
          flag(s.isFinal, 1) |
          flag(s.isClass && s.asClass.isSealed, 2) |
          flag(s.isClass && s.asClass.isCaseClass, 3) |
          flag(s.isClass && s.asClass.isTrait, 4) |
          flag(s.isModuleClass, 5)
      q"new $TypeFlagsTpe($rawFlags)"
    }
  }

  def materialize[Real: WeakTypeTag]: Tree = instrument {
    val adtTpe = weakTypeOf[Real].dealias
    val metadataTpe = c.macroApplication.tpe.dealias
    val orderedSubtypes = findAnnotation(metadataTpe.typeSymbol, AllowUnorderedSubtypesAT).isEmpty

    val adtSymbol =
      singleValueFor(adtTpe).map(sv => new AdtObject(adtTpe, 0, None, sv)) orElse
        applyUnapplyFor(adtTpe).map(au => new AdtClass(adtTpe, 0, None, au)) orElse
        knownSubtypes(adtTpe, orderedSubtypes).map(st => new AdtHierarchy(adtTpe, 0, None, st)) getOrElse
        new AdtOtherCase(adtTpe, 0, None)

    materializeMetadata(adtSymbol, metadataTpe)
  }

  def fromApplyUnapplyProvider[Real: WeakTypeTag](applyUnapplyProvider: Tree): Tree = instrument {
    val adtTpe = weakTypeOf[Real].dealias
    val metadataTpe = c.macroApplication.tpe.dealias
    val applyUnapply = applyUnapplyFor(adtTpe, applyUnapplyProvider)
      .getOrElse(abort(s"Cannot derive $metadataTpe from `apply` and `unapply`/`unapplySeq` methods of ${applyUnapplyProvider.tpe}"))
    val adtSymbol = new AdtClass(adtTpe, 0, None, applyUnapply)

    materializeMetadata(adtSymbol, metadataTpe)
  }

  def materializeMetadata(adtSymbol: AdtSymbol, metadataTpe: Type): Tree = {
    def tryMaterialize(metadataTpe: Type): Res[Tree] =
      new AdtMetadataConstructor(metadataTpe, None, None).tryMaterializeFor(adtSymbol)

    guardedMetadata(metadataTpe, adtSymbol.tpe) {
      materializeOneOf(metadataTpe)(tryMaterialize).getOrElse(err => abort(err.getOrElse("unknown error")))
    }
  }
}
