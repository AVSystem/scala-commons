package com.avsystem.commons
package rpc

import com.avsystem.commons.macros.rpc.RPCMacros

trait RawRPCCompanion[R] {
  type AsRawRPC[T] = AsRaw[T, R]
  type AsRealRPC[T] = AsReal[T, R]
  type AsRealRawRPC[T] = AsRealRaw[T, R]

  def materializeAsRaw[T]: AsRawRPC[T] = macro RPCMacros.rpcAsRaw[T, R]
  def materializeAsReal[T]: AsRealRPC[T] = macro RPCMacros.rpcAsReal[T, R]
  def materializeAsRealRaw[T]: AsRealRawRPC[T] = macro RPCMacros.rpcAsRealRaw[T, R]
}
