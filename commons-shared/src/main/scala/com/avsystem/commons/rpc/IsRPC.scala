package com.avsystem.commons
package rpc

/**
  * Typeclass that witnesses if type `T` is annotated as @RPC
  */
sealed trait IsRPC[T]
object IsRPC {
  implicit def isRPC[T]: IsRPC[T] = macro macros.rpc.RPCFrameworkMacros.isRPC[T]
}

