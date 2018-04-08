package com.avsystem.commons
package rpc

import scala.annotation.StaticAnnotation

trait AsRaw[T, R] {
  def asRaw(real: T): R
}
object AsRaw {
  def forRpc[T, R]: AsRaw[T, R] = macro macros.rpc.RPCMacros.rpcAsRaw[T, R]
}

trait AsReal[T, R] {
  def asReal(raw: R): T
}
object AsReal {
  def forRpc[T, R]: AsReal[T, R] = macro macros.rpc.RPCMacros.rpcAsReal[T, R]
}

class annotatedWith[T <: StaticAnnotation] extends StaticAnnotation