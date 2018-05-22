package com.avsystem.commons
package rpc

import com.avsystem.commons.macros.rpc.RPCMacros

trait RpcMetadataCompanion[M[_]] extends RpcImplicitsProvider {
  def materializeForRpc[T]: M[T] = macro RPCMacros.rpcMetadata[M[T], T]
}
