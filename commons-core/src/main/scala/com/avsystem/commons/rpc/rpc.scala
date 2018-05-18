package com.avsystem.commons
package rpc

import scala.annotation.implicitNotFound

@implicitNotFound("don't know how to encode ${T} as ${R}, appropriate AsRaw instance not found")
trait AsRaw[R, T] {
  def asRaw(real: T): R
}
object AsRaw {
  def create[R, T](asRawFun: T => R): AsRaw[R, T] =
    new AsRaw[R, T] {
      def asRaw(real: T): R = asRawFun(real)
    }
  def identity[A]: AsRaw[A, A] = AsRealRaw.identity[A]
  def materializeForRpc[R, T]: AsRaw[R, T] = macro macros.rpc.RPCMacros.rpcAsRaw[R, T]
}

@implicitNotFound("don't know how to decode ${R} into ${T}, appropriate AsReal instance not found")
trait AsReal[R, T] {
  def asReal(raw: R): T
}
object AsReal {
  def create[R, T](asRealFun: R => T): AsReal[R, T] =
    new AsReal[R, T] {
      def asReal(raw: R): T = asRealFun(raw)
    }
  def identity[A]: AsReal[A, A] = AsRealRaw.identity[A]
  def materializeForRpc[R, T]: AsReal[R, T] = macro macros.rpc.RPCMacros.rpcAsReal[R, T]
}

@implicitNotFound("don't know how to encode and decode between ${T} and ${R}, appropriate AsRealRaw instance not found")
trait AsRealRaw[R, T] extends AsReal[R, T] with AsRaw[R, T]
object AsRealRaw {
  def create[R, T](asRealFun: R => T, asRawFun: T => R): AsRealRaw[R, T] =
    new AsRealRaw[R, T] {
      def asRaw(real: T): R = asRawFun(real)
      def asReal(raw: R): T = asRealFun(raw)
    }

  private val reusableIdentity = new AsRealRaw[Any, Any] {
    def asReal(raw: Any): Any = raw
    def asRaw(real: Any): Any = real
  }

  def identity[A]: AsRealRaw[A, A] =
    reusableIdentity.asInstanceOf[AsRealRaw[A, A]]

  def materializeForRpc[R, T]: AsRealRaw[R, T] = macro macros.rpc.RPCMacros.rpcAsRealRaw[R, T]
}

