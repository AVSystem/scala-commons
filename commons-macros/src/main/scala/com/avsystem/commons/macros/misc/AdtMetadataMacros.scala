package com.avsystem.commons
package macros.misc

import com.avsystem.commons.macros.AbstractMacroCommons
import com.avsystem.commons.macros.rpc.{Ok, Res}

import scala.reflect.macros.blackbox

class AdtMetadataMacros(ctx: blackbox.Context) extends AbstractMacroCommons(ctx) with MacroMetadatas {

  import c.universe._

  val AdtParamMetadataAT: Type = getType(tq"$RpcPackage.adtParamMetadata")

  class AdtClass(val tpe: Type, applyUnapply: ApplyUnapply) extends MacroSymbol with MatchedSymbol {
    lazy val symbol: Symbol = tpe.dealias.typeSymbol
    def shortDescription: String = "ADT class"
    def description: String = s"$shortDescription $tpe"

    val params: List[AdtParam] =
      applyUnapply.params.map { case (sym, _) => new AdtParam(this, sym) }

    def real: MacroSymbol = this
    def annot(tpe: Type): Option[Annot] = findAnnotation(symbol, tpe)
    def allAnnots(tpe: Type): List[Annot] = allAnnotations(symbol, tpe)
    def indexInRaw: Int = 0
    def rawName: String = nameStr
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

  abstract class MetadataConstructor(val ownerType: Type, val atParam: Option[CompositeParam])
    extends BaseMetadataConstructor(primaryConstructor(ownerType, atParam)) {

    def forAdtClass: AdtClassMetadataConstructor = cast[AdtClassMetadataConstructor]
    def forAdtParam: AdtParamMetadataConstructor = cast[AdtParamMetadataConstructor]
  }

  class AdtClassMetadataConstructor(ownerType: Type, atParam: Option[CompositeParam])
    extends MetadataConstructor(ownerType, atParam) {

    val metadataParams: List[AdtParamMetadataParam] =
      sig.paramLists.flatten.map(new AdtParamMetadataParam(this, _))

    def compositeConstructor(param: CompositeParam): MetadataConstructor =
      new AdtClassMetadataConstructor(param.actualType, Some(param))

    override def paramByStrategy(paramSym: Symbol, annot: Annot): MetadataParam =
      if (annot.tpe <:< AdtParamMetadataAT) new AdtParamMetadataParam(this, paramSym)
      else super.paramByStrategy(paramSym, annot)

    def paramMappings(adtClass: AdtClass): Res[Map[AdtParamMetadataParam, Tree]] =
      collectParamMappings(adtClass.params, metadataParams, "metadata parameter")(
        (param, parser) => param.metadataFor(parser).map(t => (param, t))).map(_.toMap)

    def materializeFor(adtClass: AdtClass): Tree =
      paramMappings(adtClass).flatMap(mappings => tryMaterializeFor(adtClass, mappings)).getOrElse(abort)

    def tryMaterializeFor(adtClass: AdtClass, paramMappings: Map[AdtParamMetadataParam, Tree]): Res[Tree] =
      Res.traverse(paramLists.flatten) {
        case cmp: CompositeParam =>
          cmp.constructor.forAdtClass.tryMaterializeFor(adtClass, paramMappings).map(cmp.localValueDecl)
        case dmp: DirectMetadataParam =>
          dmp.tryMaterializeFor(adtClass).map(dmp.localValueDecl)
        case pmp: AdtParamMetadataParam =>
          Ok(pmp.localValueDecl(paramMappings(pmp)))
      }.map(constructorCall)
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
      Res.traverse(paramLists.flatten) {
        case cp: CompositeParam =>
          cp.constructor.forAdtParam.tryMaterializeFor(matchedParam).map(cp.localValueDecl)
        case dmp: DirectMetadataParam =>
          dmp.tryMaterializeFor(matchedParam).map(dmp.localValueDecl)
      }.map(constructorCall)
  }

  class AdtParamMetadataParam(owner: AdtClassMetadataConstructor, symbol: Symbol)
    extends MetadataParam(owner, symbol) with ArityParam with FilteringSymbol {

    def allowMulti: Boolean = true
    def allowNamedMulti: Boolean = true
    def allowListedMulti: Boolean = true

    val auxiliary: Boolean =
      findAnnotation(symbol, AuxiliaryAT).nonEmpty

    def pathStr: String = {
      def cpPath(cp: CompositeParam): String =
        cp.owner.forAdtClass.atParam.fold(nameStr)(cp => s"${cpPath(cp)}.$nameStr")
      owner.atParam.fold(nameStr)(cp => s"${cpPath(cp)}.$nameStr")
    }

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
    val constructor = new AdtClassMetadataConstructor(metadataTpe, None)
    val adtClass = new AdtClass(adtTpe, applyUnapplyFor(adtTpe).getOrElse(abort(s"Not an ADT class: $adtTpe")))
    guardedMetadata(metadataTpe, adtTpe)(constructor.materializeFor(adtClass))
  }
}
