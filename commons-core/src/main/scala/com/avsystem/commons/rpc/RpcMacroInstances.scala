package com.avsystem.commons
package rpc

/**
  * Base for traits that aggregate multiple RPC-related typeclass instances (e.g. `AsReal` + metadata).
  * Typically such aggregating type is declared as an implicit constructor parameter of an abstract class that
  * serves as a base class for RPC trait companions. Example: [[com.avsystem.commons.rest.DefaultRestApiCompanion]].
  * This is all in order to reduce boilerplate needed to define an RPC trait.
  *
  * A trait that extends `RpcMacroInstances[Real]` must declare abstract methods, each method taking exactly
  * one parameter of type `Implicits` and returning either an instance of `AsReal`, `AsRaw`, `AsRawReal` or
  * some RPC metadata class for RPC trait `Real`.
  *
  * If instances-trait is defined according to these rules, `RpcMacros.macroInstances` will be able to
  * automatically materialize an implementation using `AsReal.materializeForRpc`, `AsRaw.materializeForRpc`,
  * `AsRawReal.materializeForRpc` and `RpcMetadata.materializeForRpc`. The `Implicits` parameter taken by every method
  * serves to inject additional implicits into macro-materialization. The `RpcMacros.macroInstances` macro will import
  * contents of that parameter inside every method implementation.
  */
trait RpcMacroInstances[Real] {
  type Implicits
}
