package com.avsystem.commons
package rpc

trait AsRaw[T, R] {
  def asRaw(real: T): R
}

trait AsReal[T, R] {
  def asReal(raw: R): T
}
object AsReal {
  def forRpc[T, R]: AsReal[T, R] = macro macros.rpc.RPCMacros.rpcAsReal[T, R]
}
