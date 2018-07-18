package com.avsystem.commons
package rpc

import scala.annotation.implicitNotFound

@implicitNotFound("don't know how to encode ${Real} as ${Raw}, appropriate AsRaw instance not found")
trait AsRaw[Raw, Real] {
  def asRaw(real: Real): Raw
}
object AsRaw {
  def apply[Raw, Real](implicit asRaw: AsRaw[Raw, Real]): AsRaw[Raw, Real] = asRaw

  def create[Raw, Real](asRawFun: Real => Raw): AsRaw[Raw, Real] =
    new AsRaw[Raw, Real] {
      def asRaw(real: Real): Raw = asRawFun(real)
    }
  implicit def identity[A]: AsRaw[A, A] = AsRawReal.identity[A]
  def materializeForRpc[Raw, Real]: AsRaw[Raw, Real] = macro macros.rpc.RpcMacros.rpcAsRaw[Raw, Real]
}

@implicitNotFound("don't know how to decode ${Raw} into ${Real}, appropriate AsReal instance not found")
trait AsReal[Raw, Real] {
  def asReal(raw: Raw): Real
}
object AsReal {
  def apply[Raw, Real](implicit asReal: AsReal[Raw, Real]): AsReal[Raw, Real] = asReal

  def create[Raw, Real](asRealFun: Raw => Real): AsReal[Raw, Real] =
    new AsReal[Raw, Real] {
      def asReal(raw: Raw): Real = asRealFun(raw)
    }
  implicit def identity[A]: AsReal[A, A] = AsRawReal.identity[A]
  def materializeForRpc[Raw, Real]: AsReal[Raw, Real] = macro macros.rpc.RpcMacros.rpcAsReal[Raw, Real]
}

@implicitNotFound("don't know how to encode and decode between ${Real} and ${Raw}, appropriate AsRawReal instance not found")
trait AsRawReal[Raw, Real] extends AsReal[Raw, Real] with AsRaw[Raw, Real]
object AsRawReal {
  def apply[Raw, Real](implicit asRawReal: AsRawReal[Raw, Real]): AsRawReal[Raw, Real] = asRawReal

  def create[Raw, Real](asRawFun: Real => Raw, asRealFun: Raw => Real): AsRawReal[Raw, Real] =
    new AsRawReal[Raw, Real] {
      def asRaw(real: Real): Raw = asRawFun(real)
      def asReal(raw: Raw): Real = asRealFun(raw)
    }

  private val reusableIdentity = new AsRawReal[Any, Any] {
    def asReal(raw: Any): Any = raw
    def asRaw(real: Any): Any = real
  }

  implicit def identity[A]: AsRawReal[A, A] =
    reusableIdentity.asInstanceOf[AsRawReal[A, A]]

  def materializeForRpc[Raw, Real]: AsRawReal[Raw, Real] = macro macros.rpc.RpcMacros.rpcAsRawReal[Raw, Real]
}

object RpcMetadata {
  def materializeForRpc[M[_], Real]: M[Real] = macro macros.rpc.RpcMacros.rpcMetadata[M[Real], Real]
}
