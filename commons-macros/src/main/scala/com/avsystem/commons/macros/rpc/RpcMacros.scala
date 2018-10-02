package com.avsystem.commons
package macros.rpc

import com.avsystem.commons.macros.AbstractMacroCommons
import com.avsystem.commons.macros.meta.MacroSymbols

import scala.reflect.macros.blackbox

abstract class RpcMacroCommons(ctx: blackbox.Context) extends AbstractMacroCommons(ctx) with MacroSymbols {

  import c.universe._

  final def AsRealCls: Tree = tq"$RpcPackage.AsReal"
  final def AsRealObj: Tree = q"$RpcPackage.AsReal"
  final def AsRawCls: Tree = tq"$RpcPackage.AsRaw"
  final def AsRawObj: Tree = q"$RpcPackage.AsRaw"
  final def AsRawRealCls: Tree = tq"$RpcPackage.AsRawReal"
  final def AsRawRealObj: Tree = q"$RpcPackage.AsRawReal"

  final lazy val AsRealTpe: Type = getType(tq"$AsRealCls[_,_]")
  final lazy val AsRawTpe: Type = getType(tq"$AsRawCls[_,_]")
  final lazy val RpcNameAT: Type = getType(tq"$RpcPackage.rpcName")
  final lazy val RpcNameArg: Symbol = RpcNameAT.member(TermName("name"))
  final lazy val RpcNamePrefixAT: Type = getType(tq"$RpcPackage.rpcNamePrefix")
  final lazy val RpcNamePrefixArg: Symbol = RpcNamePrefixAT.member(TermName("prefix"))
  final lazy val RpcNameOverloadedOnlyArg: Symbol = RpcNamePrefixAT.member(TermName("overloadedOnly"))
  final lazy val WhenAbsentAT: Type = getType(tq"$CommonsPkg.serialization.whenAbsent[_]")
  final lazy val TransientDefaultAT: Type = getType(tq"$CommonsPkg.serialization.transientDefault")
  final lazy val MethodNameAT: Type = getType(tq"$RpcPackage.methodName")
  final lazy val RpcMethodMetadataAT: Type = getType(tq"$RpcPackage.rpcMethodMetadata")
  final lazy val RpcParamMetadataAT: Type = getType(tq"$RpcPackage.rpcParamMetadata")
  final lazy val RpcEncodingAT: Type = getType(tq"$RpcPackage.RpcEncoding")
  final lazy val VerbatimAT: Type = getType(tq"$RpcPackage.verbatim")
  final lazy val TriedAT: Type = getType(tq"$RpcPackage.tried")
  final lazy val MethodTagAT: Type = getType(tq"$RpcPackage.methodTag[_]")
  final lazy val ParamTagAT: Type = getType(tq"$RpcPackage.paramTag[_]")
  final lazy val RpcTagAT: Type = getType(tq"$RpcPackage.RpcTag")
  final lazy val RpcImplicitsSym: Symbol = getType(tq"$RpcPackage.RpcImplicitsProvider").member(TermName("implicits"))

  def registerCompanionImplicits(rawTpe: Type): Unit =
    typedCompanionOf(rawTpe).filter { companion =>
      val typed = c.typecheck(q"$companion.implicits", silent = true)
      typed != EmptyTree && typed.symbol.overrides.contains(RpcImplicitsSym)
    }.foreach { companion =>
      registerImplicitImport(q"import $companion.implicits._")
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
    val constructor = new RpcTraitMetadataConstructor(metadataTpe, None)
    guardedMetadata(metadataTpe, realRpc.tpe)(
      constructor.tryMaterializeFor(realRpc, constructor.methodMappings(realRpc)).getOrElse(abort))
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

    val instancesMethods = instancesTpe.members.iterator
      .filter(m => m.isAbstract && m.isMethod).toList.reverse

    def impl(singleMethod: Option[Symbol]): Tree = {

      val impls = instancesMethods.map { m =>
        val sig = m.typeSignatureIn(instancesTpe)
        val resultTpe = sig.finalResultType.dealias
        if (sig.typeParams.nonEmpty || sig.paramLists.nonEmpty) {
          abort(s"Problem with $m of $instancesTpe: expected non-generic, parameterless method")
        }

        val body =
          if (singleMethod.exists(_ != m))
            q"$PredefObj.???"
          else if (resultTpe <:< asRawRealTpe)
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
      }

      val implicitsName = c.freshName(TermName("implicits"))

      q"""
        new $resultTpe {
          def apply($implicitsName: $implicitsTpe, $companionReplacementName: Any): $instancesTpe = {
            import $implicitsName._
            new $instancesTpe { ..$impls; () }
          }
        }
       """
    }

    //If full implementation doesn't typecheck, find the first problematic typeclass and limit
    //compilation errors to that one in order to not overwhelm the user but rather report errors gradually
    val fullImpl = impl(None)
    c.typecheck(fullImpl, silent = true) match {
      case EmptyTree =>
        instancesMethods.iterator.map(m => impl(Some(m)))
          .find(t => c.typecheck(t, silent = true) == EmptyTree)
          .getOrElse(fullImpl)
      case t => t
    }
  }
}
