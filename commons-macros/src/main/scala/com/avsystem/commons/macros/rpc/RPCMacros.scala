package com.avsystem.commons
package macros.rpc

import com.avsystem.commons.macros.AbstractMacroCommons

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
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
  val MethodMetadataType: Type = getType(tq"$RpcPackage.MethodMetadata[_]")
  val MethodMetadatasType: Type = getType(tq"$ScalaPkg.PartialFunction[$StringCls,$MethodMetadataType]")
  val ParamMetadataType: Type = getType(tq"$RpcPackage.ParamMetadata[_]")
  val ParamMetadatasType: Type = getType(tq"$CollectionPkg.Iterable[$ParamMetadataType]")
  val NamedParamMetadatasType: Type = getType(tq"$ScalaPkg.PartialFunction[$StringCls,$ParamMetadataType]")

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

  sealed abstract class MetadataParam(val owner: MetadataConstructor, val symbol: Symbol) extends RpcParam {
    def problemStr: String = s"problem with metadata constructor parameter $nameStr of ${owner.ownerType}"
  }

  abstract class DirectMetadataParam(owner: MetadataConstructor, symbol: Symbol) extends MetadataParam(owner, symbol) {
    def materializeFor(rpcSym: RealRpcSymbol): Tree
    def tryMaterializeFor(rpcSym: RealRpcSymbol): Res[Tree]
  }

  class ImplicitParam(owner: MetadataConstructor, symbol: Symbol) extends DirectMetadataParam(owner, symbol) {
    def materializeFor(rpcSym: RealRpcSymbol): Tree =
      q"${inferCachedImplicit(actualType, s"$problemStr: ", pos)}"

    def tryMaterializeFor(rpcSym: RealRpcSymbol): Res[Tree] =
      tryInferCachedImplicit(actualType).map(n => Ok(q"$n"))
        .getOrElse(Fail(s"could not find implicit $actualType"))
  }

  class ClassParam(owner: MetadataConstructor, symbol: Symbol) extends DirectMetadataParam(owner, symbol) {
    val constructor = new DirectMetadataConstructor(actualType, this)

    def materializeFor(rpcSym: RealRpcSymbol): Tree =
      constructor.materializeFor(rpcSym)

    def tryMaterializeFor(rpcSym: RealRpcSymbol): Res[Tree] =
      constructor.tryMaterializeFor(rpcSym)
  }

  class MethodMetadataParam(owner: RpcMetadataConstructor, symbol: Symbol) extends MetadataParam(owner, symbol) {
    val perMethodType: Type = actualType.baseType(PartialFunctionClass).typeArgs(1)

    val canBuildFrom: TermName = inferCachedImplicit(
      getType(tq"$CanBuildFromCls[$NothingCls,($StringCls,$perMethodType),$actualType]"),
      s"$problemStr: ", pos
    )

    def matchErrorMsg(msg: String): String =
      s"parameter $nameStr did not match because: $msg"

    def methodMetadataConstructorFor(realMethod: RealMethod): Res[MethodMetadataConstructor] = {
      val actualMetadataType =
        perMethodType match {
          case ExistentialType(wildcards, underlying) =>
            val baseMethodResultType = underlying.baseType(MethodMetadataType.typeSymbol).typeArgs.head
            determineTypeParams(baseMethodResultType, realMethod.resultType, wildcards)
              .map(typeArgs => underlying.substituteTypes(wildcards, typeArgs))
          case t =>
            val baseMethodResultType = t.baseType(MethodMetadataType.typeSymbol).typeArgs.head
            if (baseMethodResultType =:= realMethod.resultType) Some(perMethodType) else None
        }

      actualMetadataType.map(new MethodMetadataConstructor(_, this)) match {
        case Some(res) => Ok(res)
        case None => Fail(
          s"result type ${realMethod.resultType} is incompatible with required metadata type $perMethodType")
      }
    }
  }

  private def primaryConstructor(ownerType: Type, ownerParam: Option[RpcSymbol]): Symbol =
    primaryConstructorOf(ownerType, ownerParam.fold("")(p => s"${p.problemStr}: "))

  sealed abstract class MetadataConstructor(val ownerType: Type, val symbol: Symbol) extends RpcMethod {
    override def problemStr: String = s"problem with constructor of metadata type $ownerType"

    val paramLists: List[List[MetadataParam]] =
      symbol.typeSignatureIn(ownerType).paramLists
        .map(_.map { ps =>
          if (ps.isImplicit) new ImplicitParam(this, ps)
          else createSpecializedParam(ps).getOrElse(new ClassParam(this, ps))
        })

    val plainParams: List[DirectMetadataParam] =
      paramLists.flatten.collect {
        case pmp: DirectMetadataParam => pmp
      }

    def createSpecializedParam(paramSym: Symbol): Option[MetadataParam] = None

    def constructorCall(argDecls: List[Tree]): Tree =
      q"""
        ..$argDecls
        new $ownerType(...$argLists)
      """
  }

  case class MethodMetadataMapping(realMethod: RealMethod, mdParam: MethodMetadataParam, tree: Tree)

  class RpcMetadataConstructor(ownerType: Type)
    extends MetadataConstructor(ownerType, primaryConstructor(ownerType, None)) {

    override def createSpecializedParam(paramSym: Symbol): Option[MetadataParam] =
      if (paramSym.typeSignature <:< MethodMetadatasType)
        Some(new MethodMetadataParam(this, paramSym))
      else None

    val methodMdParams: List[MethodMetadataParam] =
      paramLists.flatten.collect({ case mmp: MethodMetadataParam => mmp })

    def materializeFor(rpc: RealRpcTrait): Tree = {
      val mappings = methodMdParams.iterator
        .map(mmp => (mmp, new mutable.LinkedHashMap[String, MethodMetadataMapping]))
        .toMap

      val failedReals = new ListBuffer[String]
      def addFailure(realMethod: RealMethod, message: String): Unit = {
        errorAt(s"${realMethod.problemStr}: $message", realMethod.pos)
        failedReals += realMethod.nameStr
      }

      rpc.realMethods.foreach { realMethod =>
        val mappingResults: List[Res[MethodMetadataMapping]] = methodMdParams.map { mmp =>
          val res = for {
            mmc <- mmp.methodMetadataConstructorFor(realMethod)
            tree <- mmc.tryMaterializeFor(realMethod)
          } yield MethodMetadataMapping(realMethod, mmp, tree)
          res.mapFailure(mmp.matchErrorMsg)
        }
        mappingResults.collect({ case Ok(v) => v }) match {
          case List(single) =>
            val prevMapping = mappings(single.mdParam).put(realMethod.rpcName, single)
            if (prevMapping.nonEmpty) {
              realMethod.reportProblem(s"multiple RPCs named ${realMethod.rpcName} map to metadata parameter " +
                s"${single.mdParam.nameStr}. If you want to overload RPCs, " +
                s"disambiguate them with @rpcName annotation")
            }
          case Nil =>
            val unmatchedReport = mappingResults.iterator.collect({ case Fail(error) => s" * $error" }).mkString("\n")
            addFailure(realMethod, s"it has no matching metadata parameters:\n$unmatchedReport")
          case multiple =>
            val matchingParams = multiple.map(_.mdParam.nameStr).mkString(",")
            addFailure(realMethod, s"it has multiple matching metadata parameters: $matchingParams")
        }
      }

      if (failedReals.nonEmpty) {
        abort(s"Following real methods could not be mapped to metadata parameters: ${failedReals.mkString(",")}")
      }

      val argDecls = paramLists.flatten.map {
        case dmp: DirectMetadataParam => dmp.localValueDecl(dmp.materializeFor(rpc))
        case mmp: MethodMetadataParam => mmp.localValueDecl {
          val builderName = c.freshName(TermName("builder"))
          val statements = mappings(mmp).valuesIterator.map { mapping =>
            q"$builderName += ((${mapping.realMethod.rpcName} -> ${mapping.tree}))"
          }.toList
          q"""
            val $builderName = ${mmp.canBuildFrom}()
            $builderName.sizeHint(${statements.size})
            ..$statements
            $builderName.result()
           """
        }
      }
      constructorCall(argDecls)
    }
  }

  class MethodMetadataConstructor(ownerType: Type, ownerParam: RpcSymbol)
    extends MetadataConstructor(ownerType, primaryConstructor(ownerType, Some(ownerParam))) {

    def tryMaterializeFor(rpc: RealRpcSymbol): Res[Tree] =
      Res.traverse(plainParams)(p => p.tryMaterializeFor(rpc).map(p.localValueDecl)).map(constructorCall)
  }

  class DirectMetadataConstructor(ownerType: Type, ownerParam: RpcSymbol)
    extends MetadataConstructor(ownerType, primaryConstructor(ownerType, Some(ownerParam))) {

    def materializeFor(rpc: RealRpcSymbol): Tree =
      constructorCall(plainParams.map(p => p.localValueDecl(p.materializeFor(rpc))))

    def tryMaterializeFor(rpc: RealRpcSymbol): Res[Tree] =
      Res.traverse(plainParams)(p => p.tryMaterializeFor(rpc).map(p.localValueDecl)).map(constructorCall)
  }
}
