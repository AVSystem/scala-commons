package com.avsystem.commons
package macros.rpc

import com.avsystem.commons.macros.AbstractMacroCommons

import scala.reflect.macros.blackbox

abstract class RPCMacroCommons(ctx: blackbox.Context) extends AbstractMacroCommons(ctx) {

  import c.universe._

  val RpcPackage = q"$CommonsPkg.rpc"
  val RpcNameType: Type = getType(tq"$RpcPackage.rpcName")
  val RpcNameNameSym: Symbol = RpcNameType.member(TermName("name"))
  val AsRealCls = tq"$RpcPackage.AsReal"
  val AsRealObj = q"$RpcPackage.AsReal"
  val AsRawCls = tq"$RpcPackage.AsRaw"
  val AsRawObj = q"$RpcPackage.AsRaw"
  val AsRealRawCls = tq"$RpcPackage.AsRealRaw"
  val AsRealRawObj = q"$RpcPackage.AsRealRaw"
  val OptionLikeCls = tq"$RpcPackage.OptionLike"
  val CanBuildFromCls = tq"$CollectionPkg.generic.CanBuildFrom"

  val RpcArityAT: Type = getType(tq"$RpcPackage.RpcArity")
  val SingleArityAT: Type = getType(tq"$RpcPackage.single")
  val OptionalArityAT: Type = getType(tq"$RpcPackage.optional")
  val MultiArityAT: Type = getType(tq"$RpcPackage.multi")
  val RpcEncodingAT: Type = getType(tq"$RpcPackage.RpcEncoding")
  val VerbatimAT: Type = getType(tq"$RpcPackage.verbatim")
  val AuxiliaryAT: Type = getType(tq"$RpcPackage.auxiliary")
  val MethodTagAT: Type = getType(tq"$RpcPackage.methodTag[_,_]")
  val ParamTagAT: Type = getType(tq"$RpcPackage.paramTag[_,_]")
  val TaggedAT: Type = getType(tq"$RpcPackage.tagged[_]")
  val RpcTagAT: Type = getType(tq"$RpcPackage.RpcTag")
  val RpcImplicitsSym: Symbol = getType(tq"$RpcPackage.RpcImplicitsProvider").member(TermName("implicits"))
  val TypedMetadataType: Type = getType(tq"$RpcPackage.TypedMetadata[_]")
  val MetadataParamStrategyType: Type = getType(tq"$RpcPackage.MetadataParamStrategy")
  val ReifyAT: Type = getType(tq"$RpcPackage.reify")
  val ConstructAT: Type = getType(tq"$RpcPackage.construct")
  val InferAT: Type = getType(tq"$RpcPackage.infer")
  val MetadataParamTargetType: Type = getType(tq"$RpcPackage.MetadataParamTarget")
  val SelfAT: Type = getType(tq"$RpcPackage.self")
  val MethodsAT: Type = getType(tq"$RpcPackage.methods")
  val ParamsAT: Type = getType(tq"$RpcPackage.params")

  val NothingTpe: Type = typeOf[Nothing]
  val StringPFTpe: Type = typeOf[PartialFunction[String, Any]]
  val BIterableTpe: Type = typeOf[Iterable[Any]]
  val BIndexedSeqTpe: Type = typeOf[IndexedSeq[Any]]

  val PartialFunctionClass: Symbol = StringPFTpe.typeSymbol
  val BIterableClass: Symbol = BIterableTpe.typeSymbol
  val BIndexedSeqClass: Symbol = BIndexedSeqTpe.typeSymbol

  def registerCompanionImplicits(rawTpe: Type): Unit =
    companionOf(rawTpe).filter { companion =>
      val typed = c.typecheck(q"$companion.implicits", silent = true)
      typed != EmptyTree && typed.symbol.overrides.contains(RpcImplicitsSym)
    }.foreach { companion =>
      registerImplicitImport(q"import $companion.implicits._")
    }
}

class RPCMacros(ctx: blackbox.Context) extends RPCMacroCommons(ctx) with RPCSymbols with RPCMappings {

  import c.universe._

  def rpcAsReal[R: WeakTypeTag, T: WeakTypeTag]: Tree = {
    val raw = RawRpcTrait(weakTypeOf[R].dealias)
    val real = RealRpcTrait(weakTypeOf[T].dealias)
    val mapping = RpcMapping(real, raw, forAsRaw = false, forAsReal = true)

    // must be evaluated before `cachedImplicitDeclarations`, don't inline it into the quasiquote
    val asRealDef = mapping.asRealImpl

    q"""
      new $AsRealCls[${raw.tpe},${real.tpe}] { ${mapping.selfName}: ${TypeTree()} =>
        ..$cachedImplicitDeclarations
        $asRealDef
      }
    """
  }

  def rpcAsRaw[R: WeakTypeTag, T: WeakTypeTag]: Tree = {
    val raw = RawRpcTrait(weakTypeOf[R].dealias)
    val real = RealRpcTrait(weakTypeOf[T].dealias)
    val mapping = RpcMapping(real, raw, forAsRaw = true, forAsReal = false)

    // must be evaluated before `cachedImplicitDeclarations`, don't inline it into the quasiquote
    val asRawDef = mapping.asRawImpl

    q"""
      new $AsRawCls[${raw.tpe},${real.tpe}] { ${mapping.selfName}: ${TypeTree()} =>
        ..$cachedImplicitDeclarations
        $asRawDef
      }
     """
  }

  def rpcAsRealRaw[R: WeakTypeTag, T: WeakTypeTag]: Tree = {
    val raw = RawRpcTrait(weakTypeOf[R].dealias)
    val real = RealRpcTrait(weakTypeOf[T].dealias)
    val mapping = RpcMapping(real, raw, forAsRaw = true, forAsReal = true)

    // these two must be evaluated before `cachedImplicitDeclarations`, don't inline them into the quasiquote
    val asRealDef = mapping.asRealImpl
    val asRawDef = mapping.asRawImpl

    q"""
      new $AsRealRawCls[${raw.tpe},${real.tpe}] { ${mapping.selfName}: ${TypeTree()} =>
        ..$cachedImplicitDeclarations
        $asRealDef
        $asRawDef
      }
     """
  }

  def rpcMetadata[M: WeakTypeTag, T: WeakTypeTag]: Tree = {
    val realRpc = RealRpcTrait(weakTypeOf[T].dealias)
    val metadataTpe = weakTypeOf[M] match { // scalac, why do I have to do this?
      case TypeRef(pre, sym, Nil) =>
        internal.typeRef(pre, sym, List(realRpc.tpe))
      case TypeRef(pre, sym, List(TypeRef(NoPrefix, wc, Nil))) if wc.name == typeNames.WILDCARD =>
        internal.typeRef(pre, sym, List(realRpc.tpe))
      case t => t
    }

    val tree = new RpcMetadataConstructor(metadataTpe).materializeFor(realRpc)
    q"..$cachedImplicitDeclarations; $tree"
  }

  def lazyMetadata(metadata: Tree): Tree =
    q"${c.prefix}($metadata)"

  def actualMetadataType(baseMetadataType: Type, realType: Type, verbatim: Boolean): Res[Type] = {
    val (wildcards, underlying) = baseMetadataType match {
      case ExistentialType(wc, u) if !verbatim => (wc, u)
      case t => (Nil, t)
    }
    val baseMethodResultType = underlying.baseType(TypedMetadataType.typeSymbol).typeArgs.head

    val result = if (wildcards.isEmpty)
      Some(baseMetadataType).filter(_ => baseMethodResultType =:= realType)
    else determineTypeParams(baseMethodResultType, realType, wildcards)
      .map(typeArgs => underlying.substituteTypes(wildcards, typeArgs))

    result.map(Ok(_)).getOrElse(Fail(
      s"real type $realType is incompatible with required metadata type $baseMetadataType"))
  }

  sealed trait MetadataParamStrategy {
    def materializeFor(rpcSym: RealRpcSymbol): Tree
    def tryMaterializeFor(rpcSym: RealRpcSymbol): Res[Tree]
  }
  object MetadataParamStrategy {
    def apply(param: MetadataParam, metadataType: Type): MetadataParamStrategy = {
      if (param.strategyType <:< InferAT) InferStrategy(param, metadataType)
      else if (param.strategyType <:< ReifyAT) ReifyStrategy(param, metadataType)
      else if (param.strategyType <:< ConstructAT) ConstructStrategy(param, metadataType)
      else param.reportProblem(s"unknown metadata param strategy annotation type ${param.strategyType}")
    }

    case class InferStrategy(param: MetadataParam, metadataType: Type) extends MetadataParamStrategy {
      // method matching is based on implicit search, but param matching is not
      val escalateImplicitFailures: Boolean = param.owner match {
        case _: MethodMetadataConstructor => true
        case _ => false
      }

      def materializeFor(rpcSym: RealRpcSymbol): Tree =
        q"${param.infer(metadataType)}"

      def tryMaterializeFor(rpcSym: RealRpcSymbol): Res[Tree] =
        if (escalateImplicitFailures)
          tryInferCachedImplicit(metadataType).map(n => Ok(q"$n"))
            .getOrElse(Fail(s"no implicit value $metadataType for parameter ${param.description} could be found"))
        else
          Ok(q"${param.infer(metadataType)}")
    }

    case class ReifyStrategy(param: MetadataParam, metadataType: Type) extends MetadataParamStrategy {
      def materializeFor(rpcSym: RealRpcSymbol): Tree = ???
      def tryMaterializeFor(rpcSym: RealRpcSymbol): Res[Tree] = ???
    }

    case class ConstructStrategy(param: MetadataParam, metadataType: Type) extends MetadataParamStrategy {
      val constructor = new DirectMetadataConstructor(metadataType, param)

      def materializeFor(rpcSym: RealRpcSymbol): Tree =
        constructor.materializeFor(rpcSym)

      def tryMaterializeFor(rpcSym: RealRpcSymbol): Res[Tree] =
        constructor.tryMaterializeFor(rpcSym)
    }
  }

  sealed abstract class MetadataParam(val owner: MetadataConstructor, val symbol: Symbol) extends RpcParam {
    def shortDescription = "metadata parameter"
    def description = s"$shortDescription $nameStr of ${owner.description}"

    val strategyType: Type = annot(MetadataParamStrategyType).map(_.tpe)
      .getOrElse(if (symbol.isImplicit) InferAT else ConstructAT)
  }

  class DirectMetadataParam(owner: MetadataConstructor, symbol: Symbol) extends MetadataParam(owner, symbol) {
    val strategy: MetadataParamStrategy = MetadataParamStrategy(this, actualType)
  }

  class MethodMetadataParam(owner: RpcMetadataConstructor, symbol: Symbol)
    extends MetadataParam(owner, symbol) with RawRpcSymbol {

    def baseTag: Type = owner.baseMethodTag
    def defaultTag: Type = owner.defaultMethodTag

    val verbatimResult: Boolean =
      annot(RpcEncodingAT).exists(_.tpe <:< VerbatimAT)

    val perMethodType: Type =
      actualType.baseType(PartialFunctionClass).typeArgs(1)

    if (!(perMethodType <:< TypedMetadataType)) {
      reportProblem(s"metadata for real method must be a subtype of TypedMetadata[T] " +
        s"where T is matched against each real method's result type")
    }

    val List(baseParamTag, defaultParamTag) =
      annot(ParamTagAT).orElse(findAnnotation(perMethodType.typeSymbol, ParamTagAT))
        .map(_.tpe.baseType(ParamTagAT.typeSymbol).typeArgs)
        .getOrElse(List(owner.baseParamTag, owner.defaultParamTag))

    val canBuildFrom: TermName =
      infer(tq"$CanBuildFromCls[$NothingCls,($StringCls,$perMethodType),$actualType]")

    def mappingFor(realMethod: RealMethod): Res[MethodMetadataMapping] = for {
      mdType <- actualMetadataType(perMethodType, realMethod.resultType, verbatimResult)
      tree <- {
        if (strategyType <:< ConstructAT) new MethodMetadataConstructor(mdType, this).tryMaterializeFor(realMethod)
        else MetadataParamStrategy(this, mdType).tryMaterializeFor(realMethod)
      }
    } yield MethodMetadataMapping(realMethod, this, tree)
  }

  class ParamMetadataParam(owner: MethodMetadataConstructor, symbol: Symbol)
    extends MetadataParam(owner, symbol) with RawParamLike {

    def baseTag: Type = owner.ownerParam.baseParamTag
    def defaultTag: Type = owner.ownerParam.defaultParamTag

    def cannotMapClue = s"cannot map it to $shortDescription $nameStr of ${owner.ownerType}"

    def metadataTree(realParam: RealParam): Res[Tree] = for {
      mdType <- actualMetadataType(arity.perRealParamType, realParam.actualType, verbatim)
      tree <- MetadataParamStrategy(this, mdType).tryMaterializeFor(realParam)
    } yield tree

    def mappingFor(parser: ParamsParser): Res[ParamMetadataMapping] = arity match {
      case _: RpcArity.Single =>
        parser.extractSingle(this, metadataTree).map(t => ParamMetadataMapping.Single(this, t))
      case _: RpcArity.Optional =>
        Ok(ParamMetadataMapping.Optional(this, parser.extractOptional(this, metadataTree)))
      case _: RpcArity.IndexedMulti | _: RpcArity.IterableMulti =>
        parser.extractMulti(this, metadataTree, named = false).map(ParamMetadataMapping.Multi(this, _))
      case _: RpcArity.NamedMulti =>
        parser.extractMulti(this, rp => metadataTree(rp).map(t => q"(${rp.rpcName}, $t)"), named = true)
          .map(ParamMetadataMapping.Multi(this, _))
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

    lazy val paramLists: List[List[MetadataParam]] =
      symbol.typeSignatureIn(ownerType).paramLists.map(_.map { ps =>
        def failOnUnexpected(annotTpe: Type) =
          reportProblem(s"unexpected metadata parameter annotation type $annotTpe")

        val targetType = findAnnotation(ps, MetadataParamTargetType).map(_.tpe).getOrElse(SelfAT)
        if (targetType <:< SelfAT) new DirectMetadataParam(this, ps)
        else if (targetType <:< MethodsAT) this match {
          case rmc: RpcMetadataConstructor => new MethodMetadataParam(rmc, ps)
          case _ => failOnUnexpected(targetType)
        }
        else if (targetType <:< ParamsAT) this match {
          case mmc: MethodMetadataConstructor => new ParamMetadataParam(mmc, ps)
          case _ => failOnUnexpected(targetType)
        }
        else failOnUnexpected(targetType)
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
        case dmp: DirectMetadataParam => dmp.localValueDecl(dmp.strategy.materializeFor(rpc))
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

  sealed trait ParamMetadataMapping {
    def param: ParamMetadataParam
    def materialize: Tree
  }
  object ParamMetadataMapping {
    case class Single(param: ParamMetadataParam, metadata: Tree) extends ParamMetadataMapping {
      def materialize: Tree = metadata
    }
    case class Optional(param: ParamMetadataParam, metadataOpt: Option[Tree]) extends ParamMetadataMapping {
      def materialize: Tree =
        metadataOpt.fold[Tree](q"${param.optionLike}.none")(t => q"${param.optionLike}.some($t)")
    }
    case class Multi(param: ParamMetadataParam, metadatas: List[Tree]) extends ParamMetadataMapping {
      def materialize: Tree = {
        val builderName = c.freshName(TermName("builder"))
        q"""
          val $builderName = ${param.canBuildFrom}()
          $builderName.sizeHint(${metadatas.size})
          ..${metadatas.map(t => q"$builderName += $t")}
          $builderName.result()
         """
      }
    }
  }

  class MethodMetadataConstructor(val ownerType: Type, val ownerParam: MethodMetadataParam)
    extends MetadataConstructor(primaryConstructor(ownerType, Some(ownerParam))) {

    lazy val paramMdParams: List[ParamMetadataParam] =
      paramLists.flatten.collect({ case pmp: ParamMetadataParam => pmp })

    override def description: String =
      s"${super.description} at ${ownerParam.description}"

    def tryMaterializeFor(realMethod: RealMethod): Res[Tree] =
      for {
        paramMappings <- collectParamMappings(paramMdParams, "metadata parameter", realMethod)(_.mappingFor(_))
        metadataByParam = paramMappings.iterator.map(pmm => (pmm.param, pmm.materialize)).toMap
        argDecls <- Res.traverse(paramLists.flatten) {
          case dmp: DirectMetadataParam =>
            dmp.strategy.tryMaterializeFor(realMethod).map(dmp.localValueDecl)
          case pmp: ParamMetadataParam =>
            Ok(pmp.localValueDecl(metadataByParam(pmp)))
          case _: MethodMetadataParam =>
            abort("(bug) unexpected @params param in metadata for RPC trait")
        }
      } yield constructorCall(argDecls)
  }

  class DirectMetadataConstructor(val ownerType: Type, val ownerParam: MetadataParam)
    extends MetadataConstructor(primaryConstructor(ownerType, Some(ownerParam))) {

    override def description: String =
      s"${super.description} at ${ownerParam.description}"

    def materializeFor(rpc: RealRpcSymbol): Tree =
      constructorCall(plainParams.map(p => p.localValueDecl(p.strategy.materializeFor(rpc))))

    def tryMaterializeFor(rpc: RealRpcSymbol): Res[Tree] =
      Res.traverse(plainParams)(p => p.strategy.tryMaterializeFor(rpc).map(p.localValueDecl)).map(constructorCall)
  }
}
