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
  val RpcMetadataParamTypeAT: Type = getType(tq"$RpcPackage.RpcMetadataParamType")
  val InferAT: Type = getType(tq"$RpcPackage.infer")

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
    val metadataTpe = weakTypeOf[M] match {
      case TypeRef(pre, sym, Nil) => // scalac, why do I have to do this?
        internal.typeRef(pre, sym, List(realRpc.tpe))
      case t => t
    }
    val constr = MetadataConstructor(metadataTpe, None)
    constr.materializeFor(realRpc)
  }

  sealed abstract class MetadataParam extends RpcParam {
    val owner: MetadataConstructor

    def materializeFor(sym: RealRpcSymbol): Tree
    def tryMaterializeFor(sym: RealRpcSymbol): Res[Tree]

    def problemStr: String = s"problem with metadata parameter $nameStr of ${owner.ownerType}"
    def matchFailure(msg: String) =
      Fail(s"metadata param $nameStr did not match because")
  }

  case class TypeclassParam(owner: MetadataConstructor, symbol: Symbol) extends MetadataParam {
    val (wildcard, underlying) = actualType match {
      case ExistentialType(List(w), u) => (w, u)
      case _ => reportProblem(s"bad typeclass base type $actualType - " +
        "it must be an existential type (wildcard) with single unknown type argument")
    }

    def typeclassFor(tpe: Type): Type =
      underlying.substituteTypes(List(wildcard), List(tpe))

    def inferFor(sym: RealRpcSymbol): TermName =
      inferCachedImplicit(typeclassFor(sym.typeForMetadata), s"${sym.problemStr}: ", sym.pos)

    def tryInferFor(sym: RealRpcSymbol): Res[TermName] = {
      val typeToInfer = typeclassFor(sym.typeForMetadata)
      tryInferCachedImplicit(typeToInfer) match {
        case Some(result) => Ok(result)
        case None => matchFailure(s"could not infer $typeToInfer for result type")
      }
    }

    def materializeFor(sym: RealRpcSymbol): Tree =
      q"${inferFor(sym)}"

    def tryMaterializeFor(sym: RealRpcSymbol): Res[Tree] =
      tryInferFor(sym).map(n => q"$n")
  }

  case class CompoundParam(owner: MetadataConstructor, symbol: Symbol) extends MetadataParam {
    val constructor: MetadataConstructor =
      MetadataConstructor(actualType, Some(this))

    def materializeFor(sym: RealRpcSymbol): Tree =
      constructor.materializeFor(sym)

    def tryMaterializeFor(sym: RealRpcSymbol): Res[Tree] =
      constructor.tryMaterializeFor(sym)
  }

  object MetadataConstructor {
    def apply(ownerType: Type, ownerParam: Option[RpcSymbol]): MetadataConstructor = {
      val constrSymbol = primaryConstructorOf(ownerType, ownerParam.fold("")(p => s"${p.problemStr}: "))
      MetadataConstructor(ownerType, constrSymbol)
    }
  }
  case class MetadataConstructor(ownerType: Type, symbol: Symbol) extends RpcMethod {
    override def problemStr: String = s"problem with constructor of metadata type $ownerType"

    val paramLists: List[List[MetadataParam]] =
      symbol.typeSignatureIn(ownerType).paramLists
        .map(_.map { ps =>
          val paramTypeAnnot = findAnnotation(ps, RpcMetadataParamTypeAT)
          if (paramTypeAnnot.exists(_.tpe <:< InferAT)) TypeclassParam(this, ps)
          else CompoundParam(this, ps)
        })

    val params: List[MetadataParam] = paramLists.flatten

    def materializeFor(sym: RealRpcSymbol): Tree =
      q"""
        ..${params.map(p => p.localValueDecl(p.materializeFor(sym)))}
        new $ownerType(...$argLists)
       """

    def tryMaterializeFor(sym: RealRpcSymbol): Res[Tree] =
      Res.traverse(params)(p => p.tryMaterializeFor(sym).map(p.localValueDecl)).map { decls =>
        q"""
          ..$decls
          new $ownerType(...$argLists)
         """
      }
  }
}
