package com.avsystem.commons
package macros.rpc

import com.avsystem.commons.macros.AbstractMacroCommons

import scala.reflect.macros.blackbox

/**
  * The "legacy" macros for RPC based on `RPCFramework`.
  * Superseded by generalized, "framework-less" RPC macro engine, [[RPCMacros]].
  */
final class RPCFrameworkMacros(ctx: blackbox.Context) extends AbstractMacroCommons(ctx) {

  import c.universe._

  val RpcPackage = q"$CommonsPkg.rpc"
  val RPCFrameworkType: Type = getType(tq"$RpcPackage.RPCFramework")
  val RPCCompanionSym: Symbol = RPCFrameworkType.member(TypeName("RPCCompanion"))

  lazy val FrameworkObj: Tree = c.prefix.tree
  lazy val RawRPCCls = tq"$FrameworkObj.RawRPC"
  lazy val RPCMetadataCls = tq"$FrameworkObj.RPCMetadata"
  lazy val FullRPCInfoCls = tq"$FrameworkObj.FullRPCInfo"

  def asRealImpl[T: WeakTypeTag]: Tree =
    q"$RpcPackage.AsReal.materializeForRpc[$RawRPCCls,${weakTypeOf[T]}]"

  def asRawImpl[T: WeakTypeTag]: Tree =
    q"$RpcPackage.AsRaw.materializeForRpc[$RawRPCCls,${weakTypeOf[T]}]"

  def asRealRawImpl[T: WeakTypeTag]: Tree =
    q"$RpcPackage.AsRealRaw.materializeForRpc[$RawRPCCls,${weakTypeOf[T]}]"

  def metadataImpl[T: WeakTypeTag]: Tree =
    q"$RpcPackage.RpcMetadata.materializeForRpc[$RPCMetadataCls,${weakTypeOf[T]}]"

  def fullInfoImpl[T: WeakTypeTag]: Tree = {
    val rpcTpe = weakTypeOf[T]
    val fullRpcInfoTpe = getType(tq"$FullRPCInfoCls[$rpcTpe]").dealias
    if (!fullRpcInfoTpe.typeSymbol.isClass) {
      abort(s"Cannot materialize FullRPCInfo in $FrameworkObj because this framework does not define FullRPCInfo as trait or class")
    }
    q"""
       new $fullRpcInfoTpe {
         lazy val asRealRPC = $FrameworkObj.materializeAsReal[$rpcTpe]
         lazy val asRawRPC = $FrameworkObj.materializeAsRaw[$rpcTpe]
         lazy val metadata = $FrameworkObj.materializeMetadata[$rpcTpe]
       }
     """
  }

  /**
    * Macro that extracts `AsRealRPC`, `AsRawRPC` and `RPCMetadata` from a companion of RPC interface that extends
    * `RPCCompanion`. Macro is necessary to make sure that each callsite keeps all the type information needed
    * for ScalaJS DCE to do its job properly.
    */
  def typeClassFromFullInfo: Tree = {
    val TypeRef(frameworkTpe, _, List(rpcTpe)) = c.prefix.actualType.baseType(RPCCompanionSym)
    q"(${c.prefix}.fullRpcInfo: $frameworkTpe#FullRPCInfo[$rpcTpe]).${c.macroApplication.symbol.name.toTermName}"
  }
}
