package com.avsystem.commons
package macros.rpc

import com.avsystem.commons.macros.AbstractMacroCommons
import com.avsystem.commons.macros.meta.MacroSymbols
import com.avsystem.commons.macros.misc.Res

import scala.reflect.macros.blackbox

private[commons] abstract class RpcMacroCommons(ctx: blackbox.Context)
  extends AbstractMacroCommons(ctx) with MacroSymbols {

  import c.universe._

  final def AsRealCls: Tree = tq"$RpcPackage.AsReal"
  final def AsRealObj: Tree = q"$RpcPackage.AsReal"
  final def AsRawCls: Tree = tq"$RpcPackage.AsRaw"
  final def AsRawObj: Tree = q"$RpcPackage.AsRaw"
  final def AsRawRealCls: Tree = tq"$RpcPackage.AsRawReal"
  final def AsRawRealObj: Tree = q"$RpcPackage.AsRawReal"

  final lazy val AsRealTpe: Type = staticType(tq"$AsRealCls[_,_]")
  final lazy val AsRawTpe: Type = staticType(tq"$AsRawCls[_,_]")
  final lazy val RpcNameAT: Type = staticType(tq"$RpcPackage.rpcName")
  final lazy val RpcNameArg: Symbol = RpcNameAT.member(TermName("name"))
  final lazy val RpcNamePrefixAT: Type = staticType(tq"$RpcPackage.rpcNamePrefix")
  final lazy val RpcNamePrefixArg: Symbol = RpcNamePrefixAT.member(TermName("prefix"))
  final lazy val RpcNameOverloadedOnlyArg: Symbol = RpcNamePrefixAT.member(TermName("overloadedOnly"))
  final lazy val WhenAbsentAT: Type = staticType(tq"$CommonsPkg.serialization.whenAbsent[_]")
  final lazy val TransientDefaultAT: Type = staticType(tq"$CommonsPkg.serialization.transientDefault")
  final lazy val MethodNameAT: Type = staticType(tq"$RpcPackage.methodName")
  final lazy val RpcMethodMetadataAT: Type = staticType(tq"$RpcPackage.rpcMethodMetadata")
  final lazy val RpcParamMetadataAT: Type = staticType(tq"$RpcPackage.rpcParamMetadata")
  final lazy val RpcEncodingAT: Type = staticType(tq"$RpcPackage.RpcEncoding")
  final lazy val VerbatimAT: Type = staticType(tq"$RpcPackage.verbatim")
  final lazy val TriedAT: Type = staticType(tq"$RpcPackage.tried")
  final lazy val MethodTagAT: Type = staticType(tq"$RpcPackage.methodTag[_]")
  final lazy val ParamTagAT: Type = staticType(tq"$RpcPackage.paramTag[_]")
  final lazy val RpcTagAT: Type = staticType(tq"$RpcPackage.RpcTag")
}

private[commons] final class RpcMacros(ctx: blackbox.Context) extends RpcMacroCommons(ctx)
  with RpcSymbols with RpcMappings with RpcMetadatas {

  import c.universe._

  def rpcAsReal[Raw: WeakTypeTag, Real: WeakTypeTag]: Tree = instrument {
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

  def rpcAsRaw[Raw: WeakTypeTag, Real: WeakTypeTag]: Tree = instrument {
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

  def rpcAsRawReal[Raw: WeakTypeTag, Real: WeakTypeTag]: Tree = instrument {
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

  def rpcMetadata[Real: WeakTypeTag]: Tree = instrument {
    val realRpc = RealRpcTrait(weakTypeOf[Real].dealias)
    val metadataTpe = c.macroApplication.tpe.dealias
    val constructor = new RpcTraitMetadataConstructor(metadataTpe, None)

    def materialize: Res[Tree] = for {
      _ <- constructor.matchFilters(realRpc)
      methodMappings = constructor.methodMappings(realRpc)
      tree <- constructor.tryMaterializeFor(realRpc, methodMappings)
    } yield tree

    guardedMetadata(metadataTpe, realRpc.tpe)(materialize.getOrElse(err => abort(err.getOrElse("unknown error"))))
  }
}
