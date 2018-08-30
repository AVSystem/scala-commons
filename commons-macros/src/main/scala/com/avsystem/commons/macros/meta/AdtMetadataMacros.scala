package com.avsystem.commons
package macros.meta

import com.avsystem.commons.macros.AbstractMacroCommons
import com.avsystem.commons.macros.misc.{Fail, Ok, Res}

import scala.collection.mutable.ListBuffer
import scala.reflect.macros.blackbox

class AdtMetadataMacros(ctx: blackbox.Context) extends AbstractMacroCommons(ctx) with MacroMetadatas {

  import c.universe._

  val AdtParamMetadataAT: Type = getType(tq"$MetaPackage.adtParamMetadata")
  val AdtCaseMetadataAT: Type = getType(tq"$MetaPackage.adtCaseMetadata")

  sealed trait AdtSymbol extends MacroSymbol with SelfMatchedSymbol {
    def tpe: Type
    lazy val symbol: Symbol = tpe.dealias.typeSymbol
  }

  class AdtHierarchy(val tpe: Type, knownSubtypes: List[Type]) extends AdtSymbol {
    def shortDescription: String = "ADT hierarchy"
    def description: String = s"$shortDescription $tpe"

    val cases: List[AdtSymbol] = knownSubtypes.map { st =>
      applyUnapplyFor(st).map(au => new AdtClass(st, au)) getOrElse new AdtOtherCase(st)
    }
  }

  class AdtClass(val tpe: Type, applyUnapply: ApplyUnapply) extends AdtSymbol {
    def shortDescription: String = "ADT class"
    def description: String = s"$shortDescription $tpe"

    val params: List[AdtParam] =
      applyUnapply.params.map { case (sym, _) => new AdtParam(this, sym) }
  }

  class AdtOtherCase(val tpe: Type) extends AdtSymbol {
    def shortDescription: String = "ADT case type"
    def description: String = s"$shortDescription $nameStr"
  }

  class AdtParam(owner: AdtClass, val symbol: Symbol) extends MacroParam {
    def shortDescription: String = "ADT parameter"
    def description: String = s"$shortDescription $nameStr of ${owner.description}"
  }

  case class MatchedAdtParam(param: AdtParam, mdParam: AdtParamMetadataParam, indexInRaw: Int) extends MatchedSymbol {
    def real: MacroSymbol = param
    def annot(tpe: Type): Option[Annot] = findAnnotation(real.symbol, tpe)
    def allAnnots(tpe: Type): List[Annot] = allAnnotations(real.symbol, tpe)
    def rawName: String = param.nameStr
  }

  abstract class MetadataConstructor(ownerType: Type, atParam: Option[CompositeParam])
    extends BaseMetadataConstructor(ownerType, atParam)

  case class AdtCaseMapping(adtCase: AdtSymbol, param: AdtCaseMetadataParam, tree: Tree)

  class AdtMetadataConstructor(ownerType: Type, atParam: Option[CompositeParam])
    extends MetadataConstructor(ownerType, atParam) {

    def compositeConstructor(param: CompositeParam): MetadataConstructor =
      new AdtMetadataConstructor(param.actualType, Some(param))

    val paramMdParams: List[AdtParamMetadataParam] = collectParams[AdtParamMetadataParam]
    val caseMdParams: List[AdtCaseMetadataParam] = collectParams[AdtCaseMetadataParam]

    if (paramMdParams.nonEmpty && caseMdParams.nonEmpty) {
      reportProblem(s"having both @adtParamMetadata and @adtCaseMetadata parameters in the same class doesn't make sense")
    }

    override def paramByStrategy(paramSym: Symbol, annot: Annot): MetadataParam =
      if (annot.tpe <:< AdtParamMetadataAT) new AdtParamMetadataParam(this, paramSym)
      else if (annot.tpe <:< AdtCaseMetadataAT) new AdtCaseMetadataParam(this, paramSym)
      else super.paramByStrategy(paramSym, annot)

    def paramMappings(adtClass: AdtClass): Res[Map[AdtParamMetadataParam, Tree]] =
      collectParamMappings(adtClass.params, paramMdParams, "metadata parameter")(
        (param, parser) => param.metadataFor(parser).map(t => (param, t))).map(_.toMap)

    def tryMaterializeFor(adtClass: AdtClass, mappings: Map[AdtParamMetadataParam, Tree]): Res[Tree] =
      tryMaterialize(adtClass) {
        case pmp: AdtParamMetadataParam => Ok(mappings(pmp))
      }

    def collectCaseMappings(hierarchy: AdtHierarchy): Map[AdtCaseMetadataParam, List[AdtCaseMapping]] = {
      //TODO: need to collect these errors somehow and return Res
      val failedCases = new ListBuffer[String]
      def addFailure(adtCase: AdtSymbol, message: String): Unit = {
        errorAt(s"${adtCase.problemStr}: $message", adtCase.pos)
        failedCases += adtCase.nameStr
      }

      val mappings = hierarchy.cases.flatMap { adtCase =>
        Res.firstOk(caseMdParams) { mdParam =>
          mdParam.tryMaterializeFor(adtCase).map(t => AdtCaseMapping(adtCase, mdParam, t))
        } { errors =>
          val unmatchedReport = errors.map { case (mdParam, err) =>
            s" * ${mdParam.shortDescription} ${mdParam.nameStr} did not match: $err"
          }.mkString("\n")
          s"it has no matching metadata parameters:\n$unmatchedReport"
        } match {
          case Ok(m) => Some(m)
          case Fail(err) =>
            addFailure(adtCase, err)
            None
        }
      }

      if (failedCases.nonEmpty) {
        abort(s"Following ADT cases could not be mapped to metadata parameters: ${failedCases.mkString(",")}")
      }

      mappings.groupBy(_.param)
    }

    def tryMaterializeFor(hierarchy: AdtHierarchy, caseMappings: Map[AdtCaseMetadataParam, List[AdtCaseMapping]]): Res[Tree] =
      if (paramMdParams.nonEmpty)
        Fail(s"${hierarchy.nameStr} is not a case class")
      else tryMaterialize(hierarchy) { case acp: AdtCaseMetadataParam =>
        val mappings = caseMappings.getOrElse(acp, Nil)
        acp.arity match {
          case ParamArity.Single(_) => mappings match {
            case Nil => Fail(s"no ADT case found that would match ${acp.description}")
            case List(m) => Ok(m.tree)
            case _ => Fail(s"multiple ADT cases match ${acp.description}")
          }
          case ParamArity.Optional(_) => mappings match {
            case Nil => Ok(acp.mkOptional[Tree](None))
            case List(m) => Ok(acp.mkOptional(Some(m.tree)))
            case _ => Fail(s"multiple ADT cases match ${acp.description}")
          }
          case ParamArity.Multi(_, _) =>
            Ok(acp.mkMulti(mappings.map(m => q"(${m.adtCase.rawName}, ${m.tree})")))
        }
      }

    def tryMaterializeFor(adtSymbol: AdtSymbol): Res[Tree] = adtSymbol match {
      case hierarchy: AdtHierarchy =>
        if (paramMdParams.nonEmpty) Fail(s"${hierarchy.nameStr} is not a case class")
        else tryMaterializeFor(hierarchy, collectCaseMappings(hierarchy))
      case adtClass: AdtClass =>
        if (caseMdParams.nonEmpty) Fail(s"${adtClass.nameStr} is not a sealed hierarchy root")
        else for {
          mappings <- paramMappings(adtClass)
          res <- tryMaterializeFor(adtClass, mappings)
        } yield res
      case adtOtherCase: AdtOtherCase =>
        if (paramMdParams.nonEmpty) Fail(s"${adtOtherCase.nameStr} is not a case class")
        else if (caseMdParams.nonEmpty) Fail(s"${adtOtherCase.nameStr} is not a sealed hierarchy root")
        else tryMaterialize(adtOtherCase)(p => Fail(s"unexpected metadata param $p"))
    }
  }

  class AdtParamMetadataConstructor(ownerType: Type, atParam: Option[CompositeParam])
    extends MetadataConstructor(ownerType, atParam) {

    def compositeConstructor(param: CompositeParam): MetadataConstructor =
      new AdtParamMetadataConstructor(param.actualType, Some(param))

    override def paramByStrategy(paramSym: Symbol, annot: Annot): MetadataParam = annot.tpe match {
      case t if t <:< ReifyPositionAT => new ReifiedPositionParam(this, paramSym)
      case t if t <:< ReifyFlagsAT => new ReifiedFlagsParam(this, paramSym)
      case _ => super.paramByStrategy(paramSym, annot)
    }

    def tryMaterializeFor(matchedParam: MatchedAdtParam): Res[Tree] =
      tryMaterialize(matchedParam)(p => Fail(s"unexpected metadata parameter $p"))
  }

  class AdtCaseMetadataParam(owner: AdtMetadataConstructor, symbol: Symbol)
    extends MetadataParam(owner, symbol) with ArityParam with FilteringSymbol {

    def allowMulti: Boolean = true
    def allowNamedMulti: Boolean = true
    def allowListedMulti: Boolean = true

    def tryMaterializeFor(adtCase: AdtSymbol): Res[Tree] = for {
      _ <- matchFilters(adtCase)
      mdType <- actualMetadataType(arity.collectedType, adtCase.tpe, "data type", verbatim = false)
      tree <- new AdtMetadataConstructor(mdType, None).tryMaterializeFor(adtCase)
    } yield tree
  }

  class AdtParamMetadataParam(owner: AdtMetadataConstructor, symbol: Symbol)
    extends MetadataParam(owner, symbol) with ArityParam with FilteringSymbol {

    def allowMulti: Boolean = true
    def allowNamedMulti: Boolean = true
    def allowListedMulti: Boolean = true

    val auxiliary: Boolean =
      findAnnotation(symbol, AuxiliaryAT).nonEmpty

    private def metadataTree(adtParam: AdtParam, indexInRaw: Int): Option[Res[Tree]] =
      Some(MatchedAdtParam(adtParam, this, indexInRaw)).filter(m => matchFilters(m).isOk).map { matched =>
        val result = for {
          mdType <- actualMetadataType(arity.collectedType, adtParam.actualType, "parameter type", verbatim = false)
          tree <- new AdtParamMetadataConstructor(mdType, None).tryMaterializeFor(matched)
        } yield tree
        result.mapFailure(msg => s"${adtParam.problemStr}: cannot map it to $shortDescription $pathStr: $msg")
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

  def materialize[Real: WeakTypeTag]: Tree = {
    val adtTpe = weakTypeOf[Real].dealias
    val metadataTpe = c.macroApplication.tpe.dealias
    materializeMetadata(adtTpe, metadataTpe)
  }

  def materializeMetadata(adtTpe: Type, metadataTpe: Type): Tree = {
    val adtSymbol =
      applyUnapplyFor(adtTpe).map(au => new AdtClass(adtTpe, au)) orElse
        knownSubtypes(adtTpe).map(st => new AdtHierarchy(adtTpe, st)) getOrElse
        new AdtOtherCase(adtTpe)

    def tryMaterialize(metadataTpe: Type): Res[Tree] =
      new AdtMetadataConstructor(metadataTpe, None).tryMaterializeFor(adtSymbol)

    guardedMetadata(metadataTpe, adtTpe) {
      val res = knownSubtypes(metadataTpe).fold(tryMaterialize(metadataTpe)) { metaSubtypes =>
        Res.firstOk(metaSubtypes)(tryMaterialize) { errors =>
          val report = errors.map { case (metaSubtype, err) =>
            s" * $metaSubtype failed because: $err"
          }.mkString("\n")
          s"none of the metadata subtypes could be materialized for $adtTpe:\n$report"
        }
      }
      res.getOrElse(abort)
    }
  }

  def materializeMacroGenerated[Real: WeakTypeTag]: Tree = {
    val adtTpe = weakTypeOf[Real].dealias
    val metadataTpe = c.macroApplication.tpe.dealias.typeArgs.head
    mkMacroGenerated(metadataTpe, materializeMetadata(adtTpe, metadataTpe))
  }
}
