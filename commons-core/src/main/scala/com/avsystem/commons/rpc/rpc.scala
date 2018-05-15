package com.avsystem.commons
package rpc

import scala.annotation.implicitNotFound

@implicitNotFound("don't know how to encode ${T} as ${R}, appropriate AsRaw instance not found")
trait AsRaw[T, R] {
  def asRaw(real: T): R
}
object AsRaw {
  def identity[A]: AsRaw[A, A] = AsRealRaw.identity[A]
  def materializeForRpc[T, R]: AsRaw[T, R] = macro macros.rpc.RPCMacros.rpcAsRaw[T, R]
}

@implicitNotFound("don't know how to decode ${R} into ${T}, appropriate AsReal instance not found")
trait AsReal[T, R] {
  def asReal(raw: R): T
}
object AsReal {
  def identity[A]: AsReal[A, A] = AsRealRaw.identity[A]
  def materializeForRpc[T, R]: AsReal[T, R] = macro macros.rpc.RPCMacros.rpcAsReal[T, R]
}

@implicitNotFound("don't know how to encode and decode between ${T} and ${R}, appropriate AsRealRaw instance not found")
trait AsRealRaw[T, R] extends AsReal[T, R] with AsRaw[T, R]
object AsRealRaw {
  private val reusableIdentity = new AsRealRaw[Any, Any] {
    def asReal(raw: Any): Any = raw
    def asRaw(real: Any): Any = real
  }

  def identity[A]: AsRealRaw[A, A] =
    reusableIdentity.asInstanceOf[AsRealRaw[A, A]]

  def materializeForRpc[T, R]: AsRealRaw[T, R] = macro macros.rpc.RPCMacros.rpcAsRealRaw[T, R]
}
