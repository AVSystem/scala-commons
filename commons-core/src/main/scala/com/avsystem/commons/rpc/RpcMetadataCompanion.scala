package com.avsystem.commons
package rpc

import com.avsystem.commons.macros.rpc.RpcMacros
import com.avsystem.commons.misc.MetadataCompanion

trait RpcMetadataCompanion[M[_]] extends MetadataCompanion[M] with RpcImplicitsProvider {
  def materializeForRpc[Real]: M[Real] = macro RpcMacros.rpcMetadata[Real]
}
