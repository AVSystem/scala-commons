package com.avsystem.commons
package rpc

trait AsRawMacros { this: AsRaw.type =>
  def materialize[Raw, Real]: AsRaw[Raw, Real] = macro macros.rpc.RpcMacros.rpcAsRaw[Raw, Real]

  /**
   * Like [[materialize]] but for arbitrary real type instead of RPC trait. Scans all public methods of the real type
   * (instead of abstract methods for RPC trait). Methods can be manually excluded using
   * [[com.avsystem.commons.meta.ignore ignore]] annotation.
   */
  def materializeForApi[Raw, Real]: AsRaw[Raw, Real] = macro macros.rpc.RpcMacros.apiAsRaw[Raw, Real]
}

trait AsRealMacros { this: AsReal.type =>
  def materialize[Raw, Real]: AsReal[Raw, Real] = macro macros.rpc.RpcMacros.rpcAsReal[Raw, Real]
}

trait AsRawRealMacros { this: AsRawReal.type =>
  def materialize[Raw, Real]: AsRawReal[Raw, Real] = macro macros.rpc.RpcMacros.rpcAsRawReal[Raw, Real]
}

trait RpcMetadataMacros { this: RpcMetadata.type =>
  def materialize[M[_], Real]: M[Real] = macro macros.rpc.RpcMacros.rpcMetadata[Real]

  /**
   * Like [[materialize]] but for arbitrary real type instead of RPC trait. Scans all public methods of the real type
   * (instead of abstract methods for RPC trait). Methods can be manually excluded using
   * [[com.avsystem.commons.meta.ignore ignore]] annotation.
   */
  def materializeForApi[M[_], Real]: M[Real] = macro macros.rpc.RpcMacros.apiMetadata[Real]

  def auto[T]: T = macro macros.misc.WhiteMiscMacros.autoAnnotationMetadata
}

trait RpcUtilsMacros {
  def compilationError(error: String): Nothing = macro com.avsystem.commons.macros.misc.MiscMacros.compilationError
}

trait RawRpcMacros[Raw] {
  def materializeAsRaw[Real]: AsRaw[Raw, Real] = macro macros.rpc.RpcMacros.rpcAsRaw[Raw, Real]
  def materializeAsReal[Real]: AsReal[Raw, Real] = macro macros.rpc.RpcMacros.rpcAsReal[Raw, Real]
  def materializeAsRawReal[Real]: AsRawReal[Raw, Real] = macro macros.rpc.RpcMacros.rpcAsRawReal[Raw, Real]
  def materializeApiAsRaw[Real]: AsRaw[Raw, Real] = macro macros.rpc.RpcMacros.apiAsRaw[Raw, Real]
}

trait RPCFrameworkMacros {
  def materializeAsRaw[T]: AsRaw[_, T] = macro com.avsystem.commons.macros.rpc.RPCFrameworkMacros.asRawImpl[T]
  def materializeAsReal[T]: AsReal[_, T] = macro com.avsystem.commons.macros.rpc.RPCFrameworkMacros.asRealImpl[T]
  def materializeAsRawReal[T]: AsRawReal[_, T] = macro com.avsystem.commons.macros.rpc.RPCFrameworkMacros.asRawRealImpl[T]
  def materializeMetadata[RealRPC]: Any = macro com.avsystem.commons.macros.rpc.RPCFrameworkMacros.metadataImpl[RealRPC]
  def materializeFullInfo[T]: Any = macro com.avsystem.commons.macros.rpc.RPCFrameworkMacros.fullInfoImpl[T]
}

trait FullRPCInfoMacros {
  implicit def asRealRPC: Any = macro com.avsystem.commons.macros.rpc.RPCFrameworkMacros.typeClassFromFullInfo
  implicit def asRawRPC: Any = macro com.avsystem.commons.macros.rpc.RPCFrameworkMacros.typeClassFromFullInfo
  implicit def metadata: Any = macro com.avsystem.commons.macros.rpc.RPCFrameworkMacros.typeClassFromFullInfo
}

trait RpcMetadataCompanionMacros[M[_]] {
  def materialize[Real]: M[Real] = macro com.avsystem.commons.macros.rpc.RpcMacros.rpcMetadata[Real]
}

trait ApiMetadataCompanionMacros[M[_]] {
  def materialize[Real]: M[Real] = macro com.avsystem.commons.macros.rpc.RpcMacros.apiMetadata[Real]
}
