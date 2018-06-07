package com.avsystem.commons
package rpc

import scala.annotation.implicitNotFound

@implicitNotFound("don't know how to encode ${Real} as ${Raw}, appropriate AsRaw instance not found")
trait AsRaw[Raw, Real] {
  def asRaw(real: Real): Raw
}
object AsRaw {
  def create[Raw, Real](asRawFun: Real => Raw): AsRaw[Raw, Real] =
    new AsRaw[Raw, Real] {
      def asRaw(real: Real): Raw = asRawFun(real)
    }
  def identity[A]: AsRaw[A, A] = AsRealRaw.identity[A]
  def materializeForRpc[Raw, Real]: AsRaw[Raw, Real] = macro macros.rpc.RPCMacros.rpcAsRaw[Raw, Real]
}

@implicitNotFound("don't know how to decode ${Raw} into ${Real}, appropriate AsReal instance not found")
trait AsReal[Raw, Real] {
  def asReal(raw: Raw): Real
}
object AsReal {
  def create[Raw, Real](asRealFun: Raw => Real): AsReal[Raw, Real] =
    new AsReal[Raw, Real] {
      def asReal(raw: Raw): Real = asRealFun(raw)
    }
  def identity[A]: AsReal[A, A] = AsRealRaw.identity[A]
  def materializeForRpc[Raw, Real]: AsReal[Raw, Real] = macro macros.rpc.RPCMacros.rpcAsReal[Raw, Real]
}

@implicitNotFound("don't know how to encode and decode between ${Real} and ${Raw}, appropriate AsRealRaw instance not found")
trait AsRealRaw[Raw, Real] extends AsReal[Raw, Real] with AsRaw[Raw, Real]
object AsRealRaw {
  def create[Raw, Real](asRealFun: Raw => Real, asRawFun: Real => Raw): AsRealRaw[Raw, Real] =
    new AsRealRaw[Raw, Real] {
      def asRaw(real: Real): Raw = asRawFun(real)
      def asReal(raw: Raw): Real = asRealFun(raw)
    }

  private val reusableIdentity = new AsRealRaw[Any, Any] {
    def asReal(raw: Any): Any = raw
    def asRaw(real: Any): Any = real
  }

  def identity[A]: AsRealRaw[A, A] =
    reusableIdentity.asInstanceOf[AsRealRaw[A, A]]

  def materializeForRpc[Raw, Real]: AsRealRaw[Raw, Real] = macro macros.rpc.RPCMacros.rpcAsRealRaw[Raw, Real]
}

object RpcMetadata {
  def materializeForRpc[M[_], Real]: M[Real] = macro macros.rpc.RPCMacros.rpcMetadata[M[Real], Real]
}
