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

    def cases: List[AdtSymbol]
    def params: List[AdtParam]
  }

  class AdtHierarchy(val tpe: Type, knownSubtypes: List[Type]) extends AdtSymbol {
    def shortDescription: String = "ADT hierarchy"
    def description: String = s"$shortDescription $tpe"

    val cases: List[AdtSymbol] = knownSubtypes.map { st =>
      singleValueFor(st).map(sv => new AdtObject(st, sv)) orElse
        applyUnapplyFor(st).map(au => new AdtClass(st, au)) getOrElse
        new AdtOtherCase(st)
    }

    def params: List[AdtParam] = Nil
  }

  class AdtClass(val tpe: Type, applyUnapply: ApplyUnapply) extends AdtSymbol {
    def shortDescription: String = "ADT class"
    def description: String = s"$shortDescription $tpe"

    def cases: List[AdtSymbol] = Nil

    val params: List[AdtParam] =
      applyUnapply.params.map { case (sym, _) => new AdtParam(this, sym) }
  }

  class AdtObject(val tpe: Type, val singleValue: Tree) extends AdtSymbol {
    def shortDescription: String = "ADT object"
    def description: String = s"$shortDescription $nameStr"

    def cases: List[AdtSymbol] = Nil
    def params: List[AdtParam] = Nil
  }

  class AdtOtherCase(val tpe: Type) extends AdtSymbol {
    def shortDescription: String = "ADT case type"
    def description: String = s"$shortDescription $nameStr"

    def cases: List[AdtSymbol] = Nil
    def params: List[AdtParam] = Nil
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
      else if (annot.tpe <:< ReifyFlagsAT) new ReifiedTypeFlagsParam(this, paramSym)
      else super.paramByStrategy(paramSym, annot)

    def paramMappings(params: List[AdtParam]): Res[Map[AdtParamMetadataParam, Tree]] =
      collectParamMappings(params, paramMdParams, "metadata parameter")(
        (param, parser) => param.metadataFor(parser).map(t => (param, t))).map(_.toMap)

    def collectCaseMappings(cases: List[AdtSymbol]): Map[AdtCaseMetadataParam, List[AdtCaseMapping]] = {
      //TODO: need to collect these errors somehow and return Res
      val failedCases = new ListBuffer[String]
      def addFailure(adtCase: AdtSymbol, message: String): Unit = {
        errorAt(s"${adtCase.problemStr}: $message", adtCase.pos)
        failedCases += adtCase.nameStr
      }

      val mappings = cases.flatMap { adtCase =>
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

    def tryMaterializeFor(sym: AdtSymbol,
      caseMappings: Map[AdtCaseMetadataParam, List[AdtCaseMapping]],
      paramMappings: Map[AdtParamMetadataParam, Tree],
    ): Res[Tree] = tryMaterialize(sym) {
      case acp: AdtCaseMetadataParam =>
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
      case app: AdtParamMetadataParam =>
        Ok(paramMappings(app))
    }

    def tryMaterializeFor(adtSymbol: AdtSymbol): Res[Tree] =
      for {
        _ <- adtSymbol match {
          case _: AdtHierarchy =>
            if (paramMdParams.isEmpty) Ok(())
            else Fail(s"${adtSymbol.nameStr} is not a case class or object")
          case _ =>
            if (caseMdParams.isEmpty) Ok(())
            else Fail(s"${adtSymbol.nameStr} is not a sealed hierarchy root")
        }
        pmappings <- paramMappings(adtSymbol.params)
        cmappings = collectCaseMappings(adtSymbol.cases)
        tree <- tryMaterializeFor(adtSymbol, cmappings, pmappings)
      } yield tree
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

  class ReifiedTypeFlagsParam(owner: MetadataConstructor, symbol: Symbol) extends DirectMetadataParam(owner, symbol) {
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

  def materialize[Real: WeakTypeTag]: Tree = {
    val adtTpe = weakTypeOf[Real].dealias
    val metadataTpe = c.macroApplication.tpe.dealias
    materializeMetadata(adtTpe, metadataTpe)
  }

  def materializeMetadata(adtTpe: Type, metadataTpe: Type): Tree = {
    val adtSymbol =
      singleValueFor(adtTpe).map(sv => new AdtObject(adtTpe, sv)) orElse
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
