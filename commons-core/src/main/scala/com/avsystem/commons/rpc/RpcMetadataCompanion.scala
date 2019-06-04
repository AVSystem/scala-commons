package com.avsystem.commons
package rpc

import com.avsystem.commons.macros.rpc.RpcMacros
import com.avsystem.commons.meta.MetadataCompanion

trait RpcMetadataCompanion[M[_]] extends MetadataCompanion[M] {
  def materialize[Real]: M[Real] = macro RpcMacros.rpcMetadata[Real]
}

/**
  * Like [[RpcMetadataCompanion]] but for arbitrary real type instead of RPC trait.
  * `materialize` scans all public methods of the real type (instead of abstract methods for RPC trait).
  */
trait ApiMetadataCompanion[M[_]] extends MetadataCompanion[M] {
  def materialize[Real]: M[Real] = macro macros.rpc.RpcMacros.apiMetadata[Real]
}
