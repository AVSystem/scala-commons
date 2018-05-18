package com.avsystem.commons
package rpc

import com.avsystem.commons.macros.rpc.RPCMacros

/**
  * Base trait for companion objects of raw RPC traits.
  */
trait RawRpcCompanion[R] extends RpcImplicitsProvider {
  type AsRawRPC[T] = AsRaw[R, T]
  type AsRealRPC[T] = AsReal[R, T]
  type AsRealRawRPC[T] = AsRealRaw[R, T]

  def materializeAsRaw[T]: AsRawRPC[T] = macro RPCMacros.rpcAsRaw[R, T]
  def materializeAsReal[T]: AsRealRPC[T] = macro RPCMacros.rpcAsReal[R, T]
  def materializeAsRealRaw[T]: AsRealRawRPC[T] = macro RPCMacros.rpcAsRealRaw[R, T]
}

trait RpcImplicitsProvider {

  /**
    * If you want some more implicits to be visible by RPC macros,
    * override this `val` with an `object` and put your additional implicits into it.
    */
  val implicits: Any = null
}
