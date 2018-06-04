package com.avsystem.commons
package macros.rpc

import scala.annotation.StaticAnnotation

trait RPCMetadatas { this: RPCMacroCommons with RPCSymbols with RPCMappings =>

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

  class MethodMetadataParam(owner: RpcMetadataConstructor, symbol: Symbol)
    extends MetadataParam[RealRpcTrait](owner, symbol) with RawRpcSymbol with ArityParam {

    def allowMulti: Boolean = true
    def allowNamedMulti: Boolean = true
    def allowListedMulti: Boolean = false

    def baseTag: Type = owner.baseMethodTag
    def defaultTag: Type = owner.defaultMethodTag

    val verbatimResult: Boolean =
      annot(RpcEncodingAT).map(_.tpe <:< VerbatimAT).getOrElse(arity.verbatimByDefault)

    if (!(arity.collectedType <:< TypedMetadataType)) {
      reportProblem(s"method metadata type must be a subtype TypedMetadata[_]")
    }

    val List(baseParamTag, defaultParamTag) =
      annot(ParamTagAT).orElse(findAnnotation(arity.collectedType.typeSymbol, ParamTagAT))
        .map(_.tpe.baseType(ParamTagAT.typeSymbol).typeArgs)
        .getOrElse(List(owner.baseParamTag, owner.defaultParamTag))

    def mappingFor(realMethod: RealMethod): Res[MethodMetadataMapping] = for {
      mdType <- actualMetadataType(arity.collectedType, realMethod, verbatimResult)
      tree <- new MethodMetadataConstructor(mdType, this).tryMaterializeFor(realMethod)
    } yield MethodMetadataMapping(realMethod, this, tree)
  }

  class ParamMetadataParam(owner: MethodMetadataConstructor, symbol: Symbol)
    extends MetadataParam[RealMethod](owner, symbol) with RawParamLike {

    def baseTag: Type = owner.ownerParam.baseParamTag
    def defaultTag: Type = owner.ownerParam.defaultParamTag

    def cannotMapClue = s"cannot map it to $shortDescription $nameStr of ${owner.ownerType}"

    if (!(arity.collectedType <:< TypedMetadataType)) {
      reportProblem(s"type ${arity.collectedType} is not a subtype of TypedMetadata[_]")
    }

    private def metadataTree(realParam: RealParam, indexInRaw: Int): Res[Tree] = {
      val result = for {
        mdType <- actualMetadataType(arity.collectedType, realParam, verbatim)
        tree <- new ParamMetadataConstructor(mdType, this, indexInRaw).tryMaterializeFor(realParam)
      } yield tree
      result.mapFailure(msg => s"${realParam.problemStr}: $cannotMapClue: $msg")
    }

    def metadataFor(parser: ParamsParser): Res[Tree] = arity match {
      case _: RpcParamArity.Single =>
        parser.extractSingle(this, metadataTree(_, 0))
      case _: RpcParamArity.Optional =>
        Ok(mkOptional(parser.extractOptional(this, metadataTree(_, 0))))
      case RpcParamArity.Multi(_, true) =>
        parser.extractMulti(this, (rp, i) => metadataTree(rp, i).map(t => q"(${rp.rpcName}, $t)"), named = true).map(mkMulti(_))
      case _: RpcParamArity.Multi =>
        parser.extractMulti(this, metadataTree, named = false).map(mkMulti(_))
    }
  }

  private def primaryConstructor(ownerType: Type, ownerParam: Option[RpcSymbol]): Symbol =
    primaryConstructorOf(ownerType, ownerParam.fold("")(p => s"${p.problemStr}: "))

  sealed abstract class MetadataConstructor[Real <: RealRpcSymbol](val symbol: Symbol) extends RpcMethod {
    def ownerType: Type

    override def annot(tpe: Type): Option[Annot] =
      super.annot(tpe) orElse {
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
        val useRpcName = annot.findArg[Boolean](ReifyNameAT.member(TermName("rpcName")), Some(false))
        new ReifiedNameParam(this, paramSym, useRpcName)
      case t if t <:< HasAnnotAT =>
        new HasAnnotParam(this, paramSym, t.typeArgs.head)
      case t => reportProblem(s"metadata param strategy $t is not allowed here")
    }

    def createDefaultParam(paramSym: Symbol): MetadataParam[Real]

    lazy val paramLists: List[List[MetadataParam[Real]]] =
      symbol.typeSignatureIn(ownerType).paramLists.map(_.map { ps =>
        findAnnotation(ps, MetadataParamStrategyType).map(createDirectParam(ps, _))
          .getOrElse(if (ps.isImplicit) new ImplicitParam(this, ps) else createDefaultParam(ps))
      })

    lazy val plainParams: List[DirectMetadataParam[Real]] =
      paramLists.flatten.collect {
        case dmp: DirectMetadataParam[Real] => dmp
      }

    def constructorCall(argDecls: List[Tree]): Tree =
      q"""
        ..$argDecls
        new $ownerType(...$argLists)
      """
  }

  case class MethodMetadataMapping(realMethod: RealMethod, mdParam: MethodMetadataParam, tree: Tree)

  class RpcMetadataConstructor(val ownerType: Type)
    extends MetadataConstructor[RealRpcTrait](primaryConstructor(ownerType, None)) with RawRpcSymbol {

    def baseTag: Type = typeOf[Nothing]
    def defaultTag: Type = typeOf[Nothing]

    val List(baseMethodTag, defaultMethodTag) =
      annot(MethodTagAT)
        .map(_.tpe.baseType(MethodTagAT.typeSymbol).typeArgs)
        .getOrElse(List(NothingTpe, NothingTpe))

    val List(baseParamTag, defaultParamTag) =
      annot(ParamTagAT)
        .map(_.tpe.baseType(ParamTagAT.typeSymbol).typeArgs)
        .getOrElse(List(NothingTpe, NothingTpe))

    lazy val methodMdParams: List[MethodMetadataParam] =
      paramLists.flatten.collect({ case mmp: MethodMetadataParam => mmp })

    def createDefaultParam(paramSym: Symbol): MetadataParam[RealRpcTrait] =
      new MethodMetadataParam(this, paramSym)

    def materializeFor(rpc: RealRpcTrait): Tree = {
      val allMappings = collectMethodMappings(methodMdParams, "metadata parameters", rpc.realMethods)(_.mappingFor(_))

      val mappingsByParam = allMappings.groupBy(_.mdParam)
      mappingsByParam.foreach { case (mmp, mappings) =>
        mappings.groupBy(_.realMethod.rpcName).foreach {
          case (rpcName, MethodMetadataMapping(realMethod, _, _) :: tail) if tail.nonEmpty =>
            realMethod.reportProblem(s"multiple RPCs named $rpcName map to metadata parameter ${mmp.nameStr}. " +
              s"If you want to overload RPCs, disambiguate them with @rpcName annotation")
          case _ =>
        }
      }

      val argDecls = paramLists.flatten.map {
        case dmp: DirectMetadataParam[RealRpcTrait] => dmp.localValueDecl(dmp.materializeFor(rpc))
        case mmp: MethodMetadataParam => mmp.localValueDecl {
          val mappings = mappingsByParam.getOrElse(mmp, Nil)
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
              mmp.mkMulti(mappings.map(m => q"(${m.realMethod.rpcName}, ${m.tree})"))
          }
        }
      }
      constructorCall(argDecls)
    }
  }

  class MethodMetadataConstructor(val ownerType: Type, val ownerParam: MethodMetadataParam)
    extends MetadataConstructor[RealMethod](primaryConstructor(ownerType, Some(ownerParam))) {

    lazy val paramMdParams: List[ParamMetadataParam] =
      paramLists.flatten.collect({ case pmp: ParamMetadataParam => pmp })

    def createDefaultParam(paramSym: Symbol): MetadataParam[RealMethod] =
      new ParamMetadataParam(this, paramSym)

    def tryMaterializeFor(realMethod: RealMethod): Res[Tree] =
      for {
        paramMappings <- collectParamMappings(paramMdParams, "metadata parameter", realMethod)(
          (param, parser) => param.metadataFor(parser).map(t => (param, t))).map(_.toMap)
        argDecls <- Res.traverse(paramLists.flatten) {
          case dmp: DirectMetadataParam[RealMethod] =>
            dmp.tryMaterializeFor(realMethod).map(dmp.localValueDecl)
          case pmp: ParamMetadataParam =>
            Ok(pmp.localValueDecl(paramMappings(pmp)))
        }
      } yield constructorCall(argDecls)
  }

  class ParamMetadataConstructor(val ownerType: Type, val ownerParam: ParamMetadataParam, val indexInRaw: Int)
    extends MetadataConstructor[RealParam](primaryConstructor(ownerType, Some(ownerParam))) {

    override def description: String =
      s"${super.description} at ${ownerParam.description}"

    override def createDirectParam(paramSym: Symbol, annot: Annot): DirectMetadataParam[RealParam] =
      annot.tpe match {
        case t if t <:< ReifyPositionAT => new ReifiedPositionParam(this, paramSym)
        case t if t <:< ReifyFlagsAT => new ReifiedFlagsParam(this, paramSym)
        case _ => super.createDirectParam(paramSym, annot)
      }

    def createDefaultParam(paramSym: Symbol): MetadataParam[RealParam] =
      new UnknownParam(this, paramSym)

    def materializeFor(param: RealParam): Tree =
      constructorCall(plainParams.map(p => p.localValueDecl(p.materializeFor(param))))

    def tryMaterializeFor(param: RealParam): Res[Tree] =
      Res.traverse(plainParams)(p => p.tryMaterializeFor(param).map(p.localValueDecl)).map(constructorCall)
  }

  sealed abstract class DirectMetadataParam[Real <: RealRpcSymbol](owner: MetadataConstructor[Real], symbol: Symbol)
    extends MetadataParam[Real](owner, symbol) {

    def materializeFor(rpcSym: Real): Tree
    def tryMaterializeFor(rpcSym: Real): Res[Tree]
  }

  class ImplicitParam[Real <: RealRpcSymbol](owner: MetadataConstructor[Real], symbol: Symbol)
    extends DirectMetadataParam[Real](owner, symbol) {

    val checked: Boolean = annot(CheckedAT).nonEmpty

    def materializeFor(rpcSym: Real): Tree =
      q"${infer(actualType)}"

    def tryMaterializeFor(rpcSym: Real): Res[Tree] =
      if (checked)
        tryInferCachedImplicit(actualType).map(n => Ok(q"$n"))
          .getOrElse(Fail(s"no implicit value $actualType for parameter $description could be found"))
      else
        Ok(materializeFor(rpcSym))
  }

  class ReifiedAnnotParam[Real <: RealRpcSymbol](owner: MetadataConstructor[Real], symbol: Symbol)
    extends DirectMetadataParam[Real](owner, symbol) with ArityParam {

    def allowMulti: Boolean = true
    def allowNamedMulti: Boolean = false
    def allowListedMulti: Boolean = true

    if (!(arity.collectedType <:< typeOf[StaticAnnotation])) {
      reportProblem(s"${arity.collectedType} is not a subtype of StaticAnnotation")
    }

    def validated(annot: Annot): Annot = {
      if (containsInaccessibleThises(annot.tree)) {
        reportProblem(s"reified annotation must not contain this-references inaccessible outside RPC trait")
      }
      annot
    }

    def materializeFor(rpcSym: Real): Tree = arity match {
      case RpcParamArity.Single(annotTpe) =>
        rpcSym.annot(annotTpe).map(a => c.untypecheck(validated(a).tree)).getOrElse {
          val msg = s"${rpcSym.problemStr}: cannot materialize value for $description: no annotation of type $annotTpe found"
          q"$RpcPackage.RpcUtils.compilationError(${StringLiteral(msg, rpcSym.pos)})"
        }
      case RpcParamArity.Optional(annotTpe) =>
        mkOptional(rpcSym.annot(annotTpe).map(a => c.untypecheck(validated(a).tree)))
      case RpcParamArity.Multi(annotTpe, _) =>
        mkMulti(allAnnotations(rpcSym.symbol, annotTpe).map(a => c.untypecheck(validated(a).tree)))
    }

    def tryMaterializeFor(rpcSym: Real): Res[Tree] =
      Ok(materializeFor(rpcSym))
  }

  class HasAnnotParam[Real <: RealRpcSymbol](owner: MetadataConstructor[Real], symbol: Symbol, annotTpe: Type)
    extends DirectMetadataParam[Real](owner, symbol) {

    if (!(actualType =:= typeOf[Boolean])) {
      reportProblem("@hasAnnot can only be used on Boolean parameters")
    }

    def materializeFor(rpcSym: Real): Tree = q"${allAnnotations(rpcSym.symbol, annotTpe).nonEmpty}"
    def tryMaterializeFor(rpcSym: Real): Res[Tree] = Ok(materializeFor(rpcSym))
  }

  class ReifiedNameParam[Real <: RealRpcSymbol](owner: MetadataConstructor[Real], symbol: Symbol, useRpcName: Boolean)
    extends DirectMetadataParam[Real](owner, symbol) {

    if (!(actualType =:= typeOf[String])) {
      reportProblem(s"its type is not String")
    }

    def materializeFor(rpcSym: Real): Tree =
      q"${if (useRpcName) rpcSym.rpcName else rpcSym.nameStr}"

    def tryMaterializeFor(rpcSym: Real): Res[Tree] =
      Ok(materializeFor(rpcSym))
  }

  class ReifiedPositionParam(owner: ParamMetadataConstructor, symbol: Symbol)
    extends DirectMetadataParam[RealParam](owner, symbol) {

    if (!(actualType =:= ParamPositionTpe)) {
      reportProblem("its type is not ParamPosition")
    }

    def materializeFor(rpcSym: RealParam): Tree =
      q"$ParamPositionObj(${rpcSym.index}, ${rpcSym.indexOfList}, ${rpcSym.indexInList}, ${owner.indexInRaw})"

    def tryMaterializeFor(rpcSym: RealParam): Res[Tree] =
      Ok(materializeFor(rpcSym))
  }

  class ReifiedFlagsParam(owner: ParamMetadataConstructor, symbol: Symbol)
    extends DirectMetadataParam[RealParam](owner, symbol) {

    if (!(actualType =:= ParamFlagsTpe)) {
      reportProblem("its type is not ParamFlags")
    }

    def materializeFor(rpcSym: RealParam): Tree = {
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

    def tryMaterializeFor(rpcSym: RealParam): Res[Tree] =
      Ok(materializeFor(rpcSym))
  }

  class UnknownParam[Real <: RealRpcSymbol](owner: MetadataConstructor[Real], symbol: Symbol)
    extends DirectMetadataParam[Real](owner, symbol) {

    def materializeFor(rpcSym: Real): Tree =
      reportProblem(s"no strategy annotation (e.g. @infer) found")
    def tryMaterializeFor(rpcSym: Real): Res[Tree] =
      Ok(materializeFor(rpcSym))
  }
}
