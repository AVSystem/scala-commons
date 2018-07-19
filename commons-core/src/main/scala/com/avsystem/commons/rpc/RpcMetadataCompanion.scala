package com.avsystem.commons
package rpc

import com.avsystem.commons.macros.rpc.RpcMacros

trait RpcMetadataCompanion[M[_]] extends RpcImplicitsProvider {
  def apply[Real](implicit metadata: M[Real]): M[Real] = metadata

  def materializeForRpc[Real]: M[Real] = macro RpcMacros.rpcMetadata[Real]

  implicit def fromFallback[Real](implicit fallback: Fallback[M[Real]]): M[Real] = fallback.value

  final class Lazy[Real](metadata: => M[Real]) {
    lazy val value: M[Real] = metadata
  }
  object Lazy {
    def apply[Real](metadata: => M[Real]): Lazy[Real] = new Lazy(metadata)

    // macro effectively turns `metadata` param into by-name param (implicit params by themselves cannot be by-name)
    implicit def lazyMetadata[Real](implicit metadata: M[Real]): Lazy[Real] = macro RpcMacros.lazyMetadata
  }
}
