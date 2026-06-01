package com.avsystem.commons
package rpc

/** Base trait for companion objects of raw RPC traits.
  */
trait RawRpcCompanion[Raw] {
  type AsRawRpc[Real] = AsRaw[Raw, Real]
  type AsRealRpc[Real] = AsReal[Raw, Real]
  type AsRawRealRpc[Real] = AsRawReal[Raw, Real]

  def asRealRpc[Real](implicit asReal: AsRealRpc[Real]): AsRealRpc[Real] = asReal
  def asRawRpc[Real](implicit asRaw: AsRawRpc[Real]): AsRawRpc[Real] = asRaw
  def asRawRealRpc[Real](implicit asRawReal: AsRawRealRpc[Real]): AsRawRealRpc[Real] = asRawReal

  def asReal[Real](raw: Raw)(implicit asRealRpc: AsRealRpc[Real]): Real = asRealRpc.asReal(raw)
  def asRaw[Real](real: Real)(implicit asRawRpc: AsRawRpc[Real]): Raw = asRawRpc.asRaw(real)

  // TODO[scala3-port]: RawRpcCompanion.materializeAsRaw (Scala 2 macro def) (L)
  def materializeAsRaw[Real]: AsRawRpc[Real] = ???
  // TODO[scala3-port]: RawRpcCompanion.materializeAsReal (Scala 2 macro def) (L)
  def materializeAsReal[Real]: AsRealRpc[Real] = ???
  // TODO[scala3-port]: RawRpcCompanion.materializeAsRawReal (Scala 2 macro def) (L)
  def materializeAsRawReal[Real]: AsRawRealRpc[Real] = ???
  // TODO[scala3-port]: RawRpcCompanion.materializeApiAsRaw (Scala 2 macro def) (L)
  def materializeApiAsRaw[Real]: AsRawRpc[Real] = ???
}
