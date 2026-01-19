package com.avsystem.commons
package rpc

import com.avsystem.commons.macros.rpc.RpcMacros
import com.avsystem.commons.meta.MetadataCompanion

/** Base trait for companion objects of RPC metadata classes.
  *
  * RPC metadata class is a generic class which captures information about some RPC trait's API (its abstract methods).
  * The `materialize` macro is responsible for doing this compile-time reflection. It is steered by various
  * meta-annotations present in the definition of the metadata class, e.g. [[rpcMethodMetadata]].
  *
  * @tparam M
  *   metadata class type constructor
  */
trait RpcMetadataCompanion[M[_]] extends MetadataCompanion[M] {
  def materialize[Real]: M[Real] = macro RpcMacros.rpcMetadata[Real]
}

/** Like [[RpcMetadataCompanion]] but reflects over the entire public API of a particular Scala type (unlike RPC traits
  * which only have their abstract methods captured).
  */
trait ApiMetadataCompanion[M[_]] extends MetadataCompanion[M] {
  def materialize[Real]: M[Real] = macro macros.rpc.RpcMacros.apiMetadata[Real]
}
