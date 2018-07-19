package com.avsystem.commons
package macros.rpc

import com.avsystem.commons.macros.AbstractMacroCommons

import scala.reflect.macros.blackbox

abstract class RpcMacroCommons(ctx: blackbox.Context) extends AbstractMacroCommons(ctx) {

  import c.universe._

  val RpcPackage = q"$CommonsPkg.rpc"
  val RpcUtils = q"$RpcPackage.RpcUtils"
  val AsRealCls = tq"$RpcPackage.AsReal"
  val AsRealObj = q"$RpcPackage.AsReal"
  val AsRawCls = tq"$RpcPackage.AsRaw"
  val AsRawObj = q"$RpcPackage.AsRaw"
  val AsRawRealCls = tq"$RpcPackage.AsRawReal"
  val AsRawRealObj = q"$RpcPackage.AsRawReal"
  val OptionLikeCls = tq"$RpcPackage.OptionLike"
  val CanBuildFromCls = tq"$CollectionPkg.generic.CanBuildFrom"
  val ParamPositionObj = q"$RpcPackage.ParamPosition"

  val AsRealTpe: Type = getType(tq"$AsRealCls[_,_]")
  val AsRawTpe: Type = getType(tq"$AsRawCls[_,_]")
  val RpcNameAT: Type = getType(tq"$RpcPackage.rpcName")
  val RpcNameArg: Symbol = RpcNameAT.member(TermName("name"))
  val RpcNamePrefixAT: Type = getType(tq"$RpcPackage.rpcNamePrefix")
  val RpcNamePrefixArg: Symbol = RpcNamePrefixAT.member(TermName("prefix"))
  val WhenAbsentAT: Type = getType(tq"$CommonsPkg.serialization.whenAbsent[_]")
  val TransientDefaultAT: Type = getType(tq"$CommonsPkg.serialization.transientDefault")
  val MethodNameAT: Type = getType(tq"$RpcPackage.methodName")
  val CompositeAT: Type = getType(tq"$RpcPackage.composite")
  val RpcArityAT: Type = getType(tq"$RpcPackage.RpcArity")
  val SingleArityAT: Type = getType(tq"$RpcPackage.single")
  val OptionalArityAT: Type = getType(tq"$RpcPackage.optional")
  val MultiArityAT: Type = getType(tq"$RpcPackage.multi")
  val RpcEncodingAT: Type = getType(tq"$RpcPackage.RpcEncoding")
  val VerbatimAT: Type = getType(tq"$RpcPackage.verbatim")
  val AuxiliaryAT: Type = getType(tq"$RpcPackage.auxiliary")
  val MethodTagAT: Type = getType(tq"$RpcPackage.methodTag[_]")
  val ParamTagAT: Type = getType(tq"$RpcPackage.paramTag[_]")
  val TaggedAT: Type = getType(tq"$RpcPackage.tagged[_]")
  val WhenUntaggedArg: Symbol = TaggedAT.member(TermName("whenUntagged"))
  val RpcTagAT: Type = getType(tq"$RpcPackage.RpcTag")
  val RpcImplicitsSym: Symbol = getType(tq"$RpcPackage.RpcImplicitsProvider").member(TermName("implicits"))
  val TypedMetadataType: Type = getType(tq"$RpcPackage.TypedMetadata[_]")
  val MetadataParamStrategyType: Type = getType(tq"$RpcPackage.MetadataParamStrategy")
  val ReifyAnnotAT: Type = getType(tq"$RpcPackage.reifyAnnot")
  val IsAnnotatedAT: Type = getType(tq"$RpcPackage.isAnnotated[_]")
  val InferAT: Type = getType(tq"$RpcPackage.infer")
  val ReifyNameAT: Type = getType(tq"$RpcPackage.reifyName")
  val ReifyPositionAT: Type = getType(tq"$RpcPackage.reifyPosition")
  val ReifyFlagsAT: Type = getType(tq"$RpcPackage.reifyFlags")
  val CheckedAT: Type = getType(tq"$RpcPackage.checked")
  val ParamPositionTpe: Type = getType(tq"$RpcPackage.ParamPosition")
  val ParamFlagsTpe: Type = getType(tq"$RpcPackage.ParamFlags")

  val NothingTpe: Type = typeOf[Nothing]
  val StringPFTpe: Type = typeOf[PartialFunction[String, Any]]
  val BIterableTpe: Type = typeOf[Iterable[Any]]
  val BIndexedSeqTpe: Type = typeOf[IndexedSeq[Any]]

  val PartialFunctionClass: Symbol = StringPFTpe.typeSymbol
  val BIterableClass: Symbol = BIterableTpe.typeSymbol
  val BIndexedSeqClass: Symbol = BIndexedSeqTpe.typeSymbol

  def registerCompanionImplicits(rawTpe: Type): Unit =
    typedCompanionOf(rawTpe).filter { companion =>
      val typed = c.typecheck(q"$companion.implicits", silent = true)
      typed != EmptyTree && typed.symbol.overrides.contains(RpcImplicitsSym)
    }.foreach { companion =>
      registerImplicitImport(q"import $companion.implicits._")
    }

  def containsInaccessibleThises(tree: Tree): Boolean = tree.exists {
    case t@This(_) if !t.symbol.isPackageClass && !enclosingClasses.contains(t.symbol) => true
    case _ => false
  }
}

class RpcMacros(ctx: blackbox.Context) extends RpcMacroCommons(ctx)
  with RpcSymbols with RpcMappings with RpcMetadatas {

  import c.universe._

  def rpcAsReal[Raw: WeakTypeTag, Real: WeakTypeTag]: Tree = {
    val raw = RawRpcTrait(weakTypeOf[Raw].dealias)
    val real = RealRpcTrait(weakTypeOf[Real].dealias)
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

  def rpcAsRaw[Raw: WeakTypeTag, Real: WeakTypeTag]: Tree = {
    val raw = RawRpcTrait(weakTypeOf[Raw].dealias)
    val real = RealRpcTrait(weakTypeOf[Real].dealias)
    val mapping = RpcMapping(real, raw, forAsRaw = true, forAsReal = false)
    mapping.ensureUniqueRpcNames()

    // must be evaluated before `cachedImplicitDeclarations`, don't inline it into the quasiquote
    val asRawDef = mapping.asRawImpl

    q"""
      new $AsRawCls[${raw.tpe},${real.tpe}] { ${mapping.selfName}: ${TypeTree()} =>
        ..$cachedImplicitDeclarations
        $asRawDef
      }
     """
  }

  def rpcAsRawReal[Raw: WeakTypeTag, Real: WeakTypeTag]: Tree = {
    val raw = RawRpcTrait(weakTypeOf[Raw].dealias)
    val real = RealRpcTrait(weakTypeOf[Real].dealias)
    val mapping = RpcMapping(real, raw, forAsRaw = true, forAsReal = true)
    mapping.ensureUniqueRpcNames()

    // these two must be evaluated before `cachedImplicitDeclarations`, don't inline them into the quasiquote
    val asRealDef = mapping.asRealImpl
    val asRawDef = mapping.asRawImpl

    q"""
      new $AsRawRealCls[${raw.tpe},${real.tpe}] { ${mapping.selfName}: ${TypeTree()} =>
        ..$cachedImplicitDeclarations
        $asRealDef
        $asRawDef
      }
     """
  }

  def rpcMetadata[Real: WeakTypeTag]: Tree = {
    val realRpc = RealRpcTrait(weakTypeOf[Real].dealias)
    val metadataTpe = c.macroApplication.tpe.dealias

    val constructor = new RpcMetadataConstructor(metadataTpe, None)
    // separate object for cached implicits so that lazy vals are members instead of local variables
    val depsObj = c.freshName(TermName("deps"))
    val selfName = c.freshName(TermName("self"))

    typedCompanionOf(metadataTpe) match {
      case Some(comp) =>
        // short circuit recursive implicit searches for M.Lazy[Real]
        val lazyMetadataTpe = getType(tq"$comp.Lazy[${realRpc.tpe}]")
        val lazySelfName = c.freshName(TermName("lazySelf"))
        registerImplicit(lazyMetadataTpe, lazySelfName)
        val tree = constructor.materializeFor(realRpc, constructor.methodMappings(realRpc))

        q"""
          object $depsObj {
            var $selfName: $metadataTpe = _
            private val $lazySelfName = $comp.Lazy($selfName)
            ..$cachedImplicitDeclarations
            $selfName = $tree
          }
          $depsObj.$selfName
         """

      case None =>
        val tree = constructor.materializeFor(realRpc, constructor.methodMappings(realRpc))
        q"""
          object $depsObj {
            ..$cachedImplicitDeclarations
            val $selfName = $tree
          }
          $depsObj.$selfName
         """
    }
  }

  def macroInstances: Tree = {
    val resultTpe = c.macroApplication.tpe
    val realTpe = resultTpe.typeArgs.last
    val applySig = resultTpe.member(TermName("apply")).typeSignatureIn(resultTpe)
    val implicitsTpe = applySig.paramLists.head.head.typeSignature
    val instancesTpe = applySig.finalResultType

    if (c.macroApplication.symbol.isImplicit && c.enclosingPosition.source != realTpe.typeSymbol.pos.source) {
      abort(s"Implicit materialization of RpcMacroInstances is only allowed in the same file where RPC trait is defined ($realTpe)")
    }

    val instTs = instancesTpe.typeSymbol
    if (!(instTs.isClass && instTs.isAbstract)) {
      abort(s"Expected trait or abstract class type, got $instancesTpe")
    }

    val asRawTpe = getType(tq"$AsRawCls[_,$realTpe]")
    val asRealTpe = getType(tq"$AsRealCls[_,$realTpe]")
    val asRawRealTpe = getType(tq"$AsRawRealCls[_,$realTpe]")

    val impls = instancesTpe.members.iterator.filter(m => m.isAbstract && m.isMethod).map { m =>
      val sig = m.typeSignatureIn(instancesTpe)
      val resultTpe = sig.finalResultType.dealias
      if (sig.typeParams.nonEmpty || sig.paramLists.nonEmpty) {
        abort(s"Problem with $m: expected non-generic, parameterless method")
      }

      val body =
        if (resultTpe <:< asRawRealTpe)
          q"$AsRawRealObj.materializeForRpc[..${resultTpe.typeArgs}]"
        else if (resultTpe <:< asRawTpe)
          q"$AsRawObj.materializeForRpc[..${resultTpe.typeArgs}]"
        else if (resultTpe <:< asRealTpe)
          q"$AsRealObj.materializeForRpc[..${resultTpe.typeArgs}]"
        else resultTpe.typeArgs match {
          case List(st) if st =:= realTpe =>
            q"$RpcPackage.RpcMetadata.materializeForRpc[${resultTpe.typeConstructor}, $realTpe]"
          case _ => abort(s"Bad result type $resultTpe of $m: " +
            s"it must be an AsReal/AsRaw/AsRealRaw or RPC metadata instance for $realTpe")
        }

      q"def ${m.name.toTermName} = $body"
    }.toList

    val implicitsName = c.freshName(TermName("implicits"))

    q"""
      new $resultTpe {
        def apply($implicitsName: $implicitsTpe): $instancesTpe = {
          import $implicitsName._
          new $instancesTpe { ..$impls; () }
        }
      }
     """
  }

  def lazyMetadata(metadata: Tree): Tree =
    q"${c.prefix}($metadata)"
}
