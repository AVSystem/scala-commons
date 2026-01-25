package com.avsystem.commons
package rpc

/**
 * Base trait for companion objects of raw RPC traits.
 */
trait RawRpcCompanion[Raw] extends RawRpcMacros[Raw] {
  type AsRawRpc[Real] = AsRaw[Raw, Real]
  type AsRealRpc[Real] = AsReal[Raw, Real]
  type AsRawRealRpc[Real] = AsRawReal[Raw, Real]

  def asRealRpc[Real](implicit asReal: AsRealRpc[Real]): AsRealRpc[Real] = asReal
  def asRawRpc[Real](implicit asRaw: AsRawRpc[Real]): AsRawRpc[Real] = asRaw
  def asRawRealRpc[Real](implicit asRawReal: AsRawRealRpc[Real]): AsRawRealRpc[Real] = asRawReal

  def asReal[Real](raw: Raw)(implicit asRealRpc: AsRealRpc[Real]): Real = asRealRpc.asReal(raw)
  def asRaw[Real](real: Real)(implicit asRawRpc: AsRawRpc[Real]): Raw = asRawRpc.asRaw(real)
}
