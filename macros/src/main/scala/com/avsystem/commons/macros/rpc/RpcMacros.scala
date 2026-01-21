package com.avsystem.commons
package macros.rpc

import com.avsystem.commons.macros.AbstractMacroCommons
import com.avsystem.commons.macros.meta.MacroSymbols
import com.avsystem.commons.macros.misc.Res

import scala.reflect.macros.blackbox

private[commons] abstract class RpcMacroCommons(ctx: blackbox.Context)
  extends AbstractMacroCommons(ctx) with MacroSymbols {

  import c.universe.*

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
  final lazy val MangleOverloadsAT: Type = staticType(tq"$RpcPackage.mangleOverloads")
  final lazy val WhenAbsentAT: Type = staticType(tq"$CommonsPkg.serialization.whenAbsent[_]")
  final lazy val RawWhenAbsentAT: Type = staticType(tq"$RpcPackage.rawWhenAbsent[_]")
  final lazy val TransientDefaultAT: Type = staticType(tq"$CommonsPkg.serialization.transientDefault")
  final lazy val MethodNameAT: Type = staticType(tq"$RpcPackage.methodName")
  final lazy val RpcMethodMetadataAT: Type = staticType(tq"$RpcPackage.rpcMethodMetadata")
  final lazy val RpcParamMetadataAT: Type = staticType(tq"$RpcPackage.rpcParamMetadata")
  final lazy val RpcTypeParamMetadataAT: Type = staticType(tq"$RpcPackage.rpcTypeParamMetadata")
  final lazy val EncodingDependencyAT: Type = staticType(tq"$RpcPackage.encodingDependency")
  final lazy val RpcEncodingAT: Type = staticType(tq"$RpcPackage.RpcEncoding")
  final lazy val VerbatimAT: Type = staticType(tq"$RpcPackage.verbatim")
  final lazy val TriedAT: Type = staticType(tq"$RpcPackage.tried")
  final lazy val MethodTagAT: Type = staticType(tq"$RpcPackage.methodTag[_]")
  final lazy val EncodingInterceptorTpe = staticType(tq"$RpcPackage.EncodingInterceptor[_,_]")
  final lazy val DecodingInterceptorTpe = staticType(tq"$RpcPackage.DecodingInterceptor[_,_]")
}

private[commons] final class RpcMacros(ctx: blackbox.Context)
  extends RpcMacroCommons(ctx) with RpcSymbols with RpcMappings with RpcMetadatas {

  import c.universe.*

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
    mkAsRaw(raw, real)
  }

  def apiAsRaw[Raw: WeakTypeTag, Real: WeakTypeTag]: Tree = instrument {
    val raw = RawRpcTrait(weakTypeOf[Raw].dealias)
    val real = RealApiClass(weakTypeOf[Real].dealias)
    mkAsRaw(raw, real)
  }

  private def mkAsRaw(raw: RawRpcTrait, real: RealRpcApi): Tree = {
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

  def rpcMetadata[Real: WeakTypeTag]: Tree =
    instrument(mkMetadata(RealRpcTrait(weakTypeOf[Real].dealias)))

  def apiMetadata[Real: WeakTypeTag]: Tree =
    instrument(mkMetadata(RealApiClass(weakTypeOf[Real].dealias)))

  private def mkMetadata(real: RealRpcApi): Tree = {
    val metadataTpe = c.macroApplication.tpe.dealias
    val constructor = new RpcApiMetadataConstructor(metadataTpe, None)
    val actualReal = if (constructor.abstractsTypeParams) real.forTypeConstructor else real

    def materialize: Res[Tree] = for {
      _ <- constructor.matchTagsAndFilters(actualReal)
      tree <- constructor.tryMaterializeFor(actualReal)
    } yield {
      val tparams = actualReal.typeParams
      q"..${tparams.map(_.typeParamDecl)}; ${stripTparamRefs(tparams.map(_.symbol))(tree)}"
    }

    guardedMetadata(metadataTpe, real.tpe)(materialize.getOrElse(err => abort(err.getOrElse("unknown error"))))
  }
}
