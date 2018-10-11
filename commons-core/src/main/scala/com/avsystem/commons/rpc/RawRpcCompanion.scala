package com.avsystem.commons
package rpc

import com.avsystem.commons.macros.rpc.RpcMacros

/**
  * Base trait for companion objects of raw RPC traits.
  */
trait RawRpcCompanion[Raw] {
  type AsRawRpc[Real] = AsRaw[Raw, Real]
  type AsRealRpc[Real] = AsReal[Raw, Real]
  type AsRawRealRpc[Real] = AsRawReal[Raw, Real]

  def asReal[Real](raw: Raw)(implicit asRealRpc: AsRealRpc[Real]): Real = asRealRpc.asReal(raw)
  def asRaw[Real](real: Real)(implicit asRawRpc: AsRawRpc[Real]): Raw = asRawRpc.asRaw(real)

  def materializeAsRaw[Real]: AsRawRpc[Real] = macro RpcMacros.rpcAsRaw[Raw, Real]
  def materializeAsReal[Real]: AsRealRpc[Real] = macro RpcMacros.rpcAsReal[Raw, Real]
  def materializeAsRawReal[Real]: AsRawRealRpc[Real] = macro RpcMacros.rpcAsRawReal[Raw, Real]
}
