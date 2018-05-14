package com.avsystem.commons
package rpc

trait AsRaw[T, R] {
  def asRaw(real: T): R
}
object AsRaw {
  def identity[A]: AsRaw[A, A] =
    new AsRaw[A, A] {
      def asRaw(a: A): A = a
    }
  def forRpc[T, R]: AsRaw[T, R] = macro macros.rpc.RPCMacros.rpcAsRaw[T, R]
}

trait AsReal[T, R] {
  def asReal(raw: R): T
}
object AsReal {
  def identity[A]: AsReal[A, A] =
    new AsReal[A, A] {
      def asReal(a: A): A = a
    }
  def forRpc[T, R]: AsReal[T, R] = macro macros.rpc.RPCMacros.rpcAsReal[T, R]
}
