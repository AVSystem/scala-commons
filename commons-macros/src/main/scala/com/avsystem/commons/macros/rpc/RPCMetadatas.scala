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

  sealed abstract class MetadataParam(val owner: MetadataConstructor, val symbol: Symbol) extends RpcParam {
    def shortDescription = "metadata parameter"
    def description = s"$shortDescription $nameStr of ${owner.description}"
  }

  class MethodMetadataParam(owner: RpcMetadataConstructor, symbol: Symbol)
    extends MetadataParam(owner, symbol) with RawRpcSymbol {

    def baseTag: Type = owner.baseMethodTag
    def defaultTag: Type = owner.defaultMethodTag

    val verbatimResult: Boolean =
      annot(RpcEncodingAT).exists(_.tpe <:< VerbatimAT)

    if (!(actualType <:< MetadataPFType)) {
      reportProblem(s"its type must be a subtype of PartialFunction[String, TypedMetadata[_]]")
    }

    val perMethodType: Type =
      actualType.baseType(PartialFunctionClass).typeArgs(1)

    val List(baseParamTag, defaultParamTag) =
      annot(ParamTagAT).orElse(findAnnotation(perMethodType.typeSymbol, ParamTagAT))
        .map(_.tpe.baseType(ParamTagAT.typeSymbol).typeArgs)
        .getOrElse(List(owner.baseParamTag, owner.defaultParamTag))

    val canBuildFrom: TermName =
      infer(tq"$CanBuildFromCls[$NothingCls,($StringCls,$perMethodType),$actualType]")

    def mappingFor(realMethod: RealMethod): Res[MethodMetadataMapping] = for {
      mdType <- actualMetadataType(perMethodType, realMethod, verbatimResult)
      tree <- new MethodMetadataConstructor(mdType, this).tryMaterializeFor(realMethod)
    } yield MethodMetadataMapping(realMethod, this, tree)
  }

  class ParamMetadataParam(owner: MethodMetadataConstructor, symbol: Symbol)
    extends MetadataParam(owner, symbol) with RawParamLike {

    def baseTag: Type = owner.ownerParam.baseParamTag
    def defaultTag: Type = owner.ownerParam.defaultParamTag

    def cannotMapClue = s"cannot map it to $shortDescription $nameStr of ${owner.ownerType}"

    if (!(arity.collectedType <:< TypedMetadataType)) {
      reportProblem(s"type ${arity.collectedType} is not a subtype of TypedMetadata[_]")
    }

    private def metadataTree(realParam: RealParam): Res[Tree] = {
      val result = for {
        mdType <- actualMetadataType(arity.collectedType, realParam, verbatim)
        tree <- new DirectMetadataConstructor(mdType, this).tryMaterializeFor(realParam)
      } yield tree
      result.mapFailure(msg => s"${realParam.problemStr}: $cannotMapClue: $msg")
    }

    def metadataFor(parser: ParamsParser): Res[Tree] = arity match {
      case _: RpcArity.Single =>
        parser.extractSingle(this, metadataTree)
      case _: RpcArity.Optional =>
        Ok(mkOptional(parser.extractOptional(this, metadataTree)))
      case RpcArity.Multi(_, true) =>
        parser.extractMulti(this, rp => metadataTree(rp).map(t => q"(${rp.rpcName}, $t)"), named = true).map(mkMulti(_))
      case _: RpcArity.Multi =>
        parser.extractMulti(this, metadataTree, named = false).map(mkMulti(_))
    }
  }

  private def primaryConstructor(ownerType: Type, ownerParam: Option[RpcSymbol]): Symbol =
    primaryConstructorOf(ownerType, ownerParam.fold("")(p => s"${p.problemStr}: "))

  sealed abstract class MetadataConstructor(val symbol: Symbol) extends RpcMethod {
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

    def defaultMetadataParam(paramSym: Symbol): MetadataParam

    lazy val paramLists: List[List[MetadataParam]] =
      symbol.typeSignatureIn(ownerType).paramLists.map(_.map { ps =>
        findAnnotation(ps, MetadataParamStrategyType)
          .map(_.tpe).orElse(if (ps.isImplicit) Some(InferAT) else None)
          .fold(defaultMetadataParam(ps)) {
            case t if t <:< InferAT => new ImplicitParam(this, ps)
            case t if t <:< ReifyAT => new ReifiedParam(this, ps)
            case t if t <:< ReifyRpcNameAT => new ReifiedRpcNameParam(this, ps)
            case t => reportProblem(s"Unrecognized metadata param strategy type: $t")
          }
      })

    lazy val plainParams: List[DirectMetadataParam] =
      paramLists.flatten.collect {
        case pmp: DirectMetadataParam => pmp
      }

    def constructorCall(argDecls: List[Tree]): Tree =
      q"""
        ..$argDecls
        new $ownerType(...$argLists)
      """
  }

  case class MethodMetadataMapping(realMethod: RealMethod, mdParam: MethodMetadataParam, tree: Tree)

  class RpcMetadataConstructor(val ownerType: Type)
    extends MetadataConstructor(primaryConstructor(ownerType, None)) with RawRpcSymbol {

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

    def defaultMetadataParam(paramSym: Symbol): MetadataParam =
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
        case dmp: DirectMetadataParam => dmp.localValueDecl(dmp.materializeFor(rpc))
        case mmp: MethodMetadataParam => mmp.localValueDecl {
          val builderName = c.freshName(TermName("builder"))
          val statements = mappingsByParam.getOrElse(mmp, Nil).iterator.map { mapping =>
            q"$builderName += ((${mapping.realMethod.rpcName} -> ${mapping.tree}))"
          }.toList
          q"""
            val $builderName = ${mmp.canBuildFrom}()
            $builderName.sizeHint(${statements.size})
            ..$statements
            $builderName.result()
           """
        }
        case _: ParamMetadataParam =>
          abort("(bug) unexpected @forParams param in metadata for RPC trait")
      }
      constructorCall(argDecls)
    }
  }

  class MethodMetadataConstructor(val ownerType: Type, val ownerParam: MethodMetadataParam)
    extends MetadataConstructor(primaryConstructor(ownerType, Some(ownerParam))) {

    lazy val paramMdParams: List[ParamMetadataParam] =
      paramLists.flatten.collect({ case pmp: ParamMetadataParam => pmp })

    def defaultMetadataParam(paramSym: Symbol): MetadataParam =
      new ParamMetadataParam(this, paramSym)

    def tryMaterializeFor(realMethod: RealMethod): Res[Tree] =
      for {
        paramMappings <- collectParamMappings(paramMdParams, "metadata parameter", realMethod)(
          (param, parser) => param.metadataFor(parser).map(t => (param, t))).map(_.toMap)
        argDecls <- Res.traverse(paramLists.flatten) {
          case dmp: DirectMetadataParam =>
            dmp.tryMaterializeFor(realMethod).map(dmp.localValueDecl)
          case pmp: ParamMetadataParam =>
            Ok(pmp.localValueDecl(paramMappings(pmp)))
          case _: MethodMetadataParam =>
            abort("(bug) unexpected @params param in metadata for RPC trait")
        }
      } yield constructorCall(argDecls)
  }

  class DirectMetadataConstructor(val ownerType: Type, val ownerParam: MetadataParam)
    extends MetadataConstructor(primaryConstructor(ownerType, Some(ownerParam))) {

    override def description: String =
      s"${super.description} at ${ownerParam.description}"

    def defaultMetadataParam(paramSym: Symbol): MetadataParam =
      new UnknownParam(this, paramSym)

    def materializeFor(rpc: RealRpcSymbol): Tree =
      constructorCall(plainParams.map(p => p.localValueDecl(p.materializeFor(rpc))))

    def tryMaterializeFor(rpc: RealRpcSymbol): Res[Tree] =
      Res.traverse(plainParams)(p => p.tryMaterializeFor(rpc).map(p.localValueDecl)).map(constructorCall)
  }

  sealed abstract class DirectMetadataParam(owner: MetadataConstructor, symbol: Symbol) extends MetadataParam(owner, symbol) {
    def materializeFor(rpcSym: RealRpcSymbol): Tree
    def tryMaterializeFor(rpcSym: RealRpcSymbol): Res[Tree]
  }

  class ImplicitParam(owner: MetadataConstructor, symbol: Symbol) extends DirectMetadataParam(owner, symbol) {
    val checked: Boolean = annot(CheckedAT).nonEmpty

    def materializeFor(rpcSym: RealRpcSymbol): Tree =
      q"${infer(actualType)}"

    def tryMaterializeFor(rpcSym: RealRpcSymbol): Res[Tree] =
      if (checked)
        tryInferCachedImplicit(actualType).map(n => Ok(q"$n"))
          .getOrElse(Fail(s"no implicit value $actualType for parameter $description could be found"))
      else
        Ok(materializeFor(rpcSym))
  }

  class ReifiedParam(owner: MetadataConstructor, symbol: Symbol)
    extends DirectMetadataParam(owner, symbol) with ArityParam {

    def allowNamedMulti: Boolean = false

    if (!(arity.collectedType <:< typeOf[StaticAnnotation])) {
      reportProblem(s"${arity.collectedType} is not a subtype of StaticAnnotation")
    }

    def materializeFor(rpcSym: RealRpcSymbol): Tree = arity match {
      case RpcArity.Single(annotTpe) =>
        rpcSym.annot(annotTpe).map(a => c.untypecheck(a.tree)).getOrElse {
          val msg = s"${rpcSym.problemStr}: cannot materialize value for $description: no annotation of type $annotTpe found"
          q"$RpcPackage.RpcUtils.compilationError(${StringLiteral(msg, rpcSym.pos)})"
        }
      case RpcArity.Optional(annotTpe) =>
        mkOptional(rpcSym.annot(annotTpe).map(a => c.untypecheck(a.tree)))
      case RpcArity.Multi(annotTpe, _) =>
        mkMulti(allAnnotations(rpcSym.symbol, annotTpe).map(a => c.untypecheck(a.tree)))
    }

    def tryMaterializeFor(rpcSym: RealRpcSymbol): Res[Tree] =
      Ok(materializeFor(rpcSym))
  }

  class ReifiedRpcNameParam(owner: MetadataConstructor, symbol: Symbol)
    extends DirectMetadataParam(owner, symbol) {

    if (!(actualType =:= typeOf[String])) {
      reportProblem(s"its type is not String")
    }

    def materializeFor(rpcSym: RealRpcSymbol): Tree =
      q"${rpcSym.rpcName}"

    def tryMaterializeFor(rpcSym: RealRpcSymbol): Res[Tree] =
      Ok(materializeFor(rpcSym))
  }

  class UnknownParam(owner: MetadataConstructor, symbol: Symbol) extends DirectMetadataParam(owner, symbol) {
    def materializeFor(rpcSym: RealRpcSymbol): Tree =
      reportProblem(s"no strategy annotation (e.g. @infer) found")
    def tryMaterializeFor(rpcSym: RealRpcSymbol): Res[Tree] =
      Ok(materializeFor(rpcSym))
  }
}
