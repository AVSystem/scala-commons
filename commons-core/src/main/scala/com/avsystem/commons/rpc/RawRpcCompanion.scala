package com.avsystem.commons
package rpc

import com.avsystem.commons.macros.rpc.RPCMacros

/**
  * Base trait for companion objects of raw RPC traits.
  */
trait RawRpcCompanion[R] extends RpcImplicitsProvider {
  type AsRawRpc[T] = AsRaw[R, T]
  type AsRealRpc[T] = AsReal[R, T]
  type AsRealRawRpc[T] = AsRealRaw[R, T]

  def materializeAsRaw[T]: AsRawRpc[T] = macro RPCMacros.rpcAsRaw[R, T]
  def materializeAsReal[T]: AsRealRpc[T] = macro RPCMacros.rpcAsReal[R, T]
  def materializeAsRealRaw[T]: AsRealRawRpc[T] = macro RPCMacros.rpcAsRealRaw[R, T]
}


