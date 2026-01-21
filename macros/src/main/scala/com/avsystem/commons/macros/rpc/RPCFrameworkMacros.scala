package com.avsystem.commons
package macros.rpc

import com.avsystem.commons.macros.AbstractMacroCommons

import scala.reflect.macros.blackbox

/**
 * The "legacy" macros for RPC based on `RPCFramework`. Superseded by generalized, "framework-less" RPC macro engine,
 * `RpcMacros`.
 */
final class RPCFrameworkMacros(ctx: blackbox.Context) extends AbstractMacroCommons(ctx) {

  import c.universe.*

  def RpcPackage: Tree = q"$CommonsPkg.rpc"
  lazy val RPCFrameworkType: Type = staticType(tq"$RpcPackage.RPCFramework")
  lazy val RPCCompanionSym: Symbol = RPCFrameworkType.member(TypeName("RPCCompanion"))

  def FrameworkObj: Tree = c.prefix.tree
  def RawRPCCls: Tree = tq"$FrameworkObj.RawRPC"
  def RPCMetadataCls: Tree = tq"$FrameworkObj.RPCMetadata"
  def FullRPCInfoCls: Tree = tq"$FrameworkObj.FullRPCInfo"

  def asRealImpl[T: WeakTypeTag]: Tree =
    q"$RpcPackage.AsReal.materialize[$RawRPCCls,${weakTypeOf[T]}]"

  def asRawImpl[T: WeakTypeTag]: Tree =
    q"$RpcPackage.AsRaw.materialize[$RawRPCCls,${weakTypeOf[T]}]"

  def AsRawRealImpl[T: WeakTypeTag]: Tree =
    q"$RpcPackage.AsRawReal.materialize[$RawRPCCls,${weakTypeOf[T]}]"

  def metadataImpl[T: WeakTypeTag]: Tree =
    q"$RpcPackage.RpcMetadata.materialize[$RPCMetadataCls,${weakTypeOf[T]}]"

  def fullInfoImpl[T: WeakTypeTag]: Tree = instrument {
    val rpcTpe = weakTypeOf[T]
    val fullRpcInfoTpe = getType(tq"$FullRPCInfoCls[$rpcTpe]").dealias
    if (!fullRpcInfoTpe.typeSymbol.isClass) {
      abort(s"Cannot materialize FullRPCInfo in $FrameworkObj because this framework does not define FullRPCInfo as trait or class")
    }
    q"""
       new $fullRpcInfoTpe {
         import $FrameworkObj._
         lazy val asRealRPC = $FrameworkObj.materializeAsReal[$rpcTpe]
         lazy val asRawRPC = $FrameworkObj.materializeAsRaw[$rpcTpe]
         lazy val metadata = $FrameworkObj.materializeMetadata[$rpcTpe]
       }
     """
  }

  /**
   * Macro that extracts `AsRealRPC`, `AsRawRPC` and `RPCMetadata` from a companion of RPC interface that extends
   * `RPCCompanion`. Macro is necessary to make sure that each callsite keeps all the type information needed for
   * ScalaJS DCE to do its job properly.
   */
  def typeClassFromFullInfo: Tree = {
    val TypeRef(frameworkTpe, _, List(rpcTpe)) = c.prefix.actualType.baseType(RPCCompanionSym)
    q"(${c.prefix}.fullRpcInfo: $frameworkTpe#FullRPCInfo[$rpcTpe]).${c.macroApplication.symbol.name.toTermName}"
  }
}
