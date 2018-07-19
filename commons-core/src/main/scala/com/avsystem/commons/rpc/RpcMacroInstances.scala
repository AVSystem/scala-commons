package com.avsystem.commons
package rpc

/**
  * Intermediate factory that creates an `InstancesTrait` for given `Real` RPC trait, based on provided `Implicits`.
  * Normally, this factory is used as implicit constructor parameter of base classes for companion objects
  * of RPC traits (e.g. [[com.avsystem.commons.rest.DefaultRestApiCompanion DefaultRestApiCompanion]]).
  * This all serves to reduce boilerplate associated with RPC trait companion declarations and makes RPC trait
  * definitions as concise as possible. It also lets the programmer easily inject additional implicits into
  * macro-materialization of RPC-related typeclasses (`AsReal`, `AsRaw`, metadata, etc.).
  *
  * An `InstancesTrait` is a trait that aggregates multiple RPC related typeclass instances for given `Real` RPC trait.
  * There is no fixed interface for `InstancesTrait`, its members are inspected by `materialize` macro and implemented
  * automatically. `InstancesTrait` must contain only parameterless abstract methods that return either
  * `AsRaw[Raw,Real]`, `AsReal[Raw,Real]`, `AsRawReal[Raw,Real]` or some RPC metadata class for `Real`.
  * The `Raw` type is arbitrary and may be different for every method. However, it must be a concrete raw RPC trait
  * so that it's further understood by the macro engine. All methods of the `InstancesTrait` are macro-implemented
  * using `AsRaw/AsReal/AsRawReal/RpcMetadata.materializeForRpc` macros.
  *
  * Example of `InstancesTrait`: [[com.avsystem.commons.rest.ClientInstances ClientInstances]]
  *
  * The `Implicits` type is typically a trait with a collection of implicit definitions whose companion object
  * implements that trait, e.g. [[com.avsystem.commons.rest.DefaultRestImplicits DefaultRestImplicits]].
  * When the macro implements `apply` method of `RpcMacroInstances` contents of `Implicits` are imported into the
  * body of `apply` and visible further by macros that materialize `InstancesTrait`.
  */
trait RpcMacroInstances[Implicits, InstancesTrait[_], Real] {
  def apply(implicits: Implicits): InstancesTrait[Real]
}
object RpcMacroInstances {
  /**
    * Materializes an instance of `RpcMacroInstances[Implicits, InstancesTrait, Real]`. This macro should not be
    * invoked directly, it should only be used to materialize implicit parameters of RPC companion base classes,
    * e.g. [[com.avsystem.commons.rest.DefaultRestApiCompanion DefaultRestApiCompanion]].
    *
    * @example
    * {{{
    *   trait SomeRawRpc { ... }
    *   class SomeMetadata[Real](...)
    *
    *   trait SomeInstances[Real} {
    *     def asReal: AsReal[SomeRawRpc, Real]
    *     def metadata: SomeMetadata[Real]
    *   }
    *
    *   trait SomeImplicits { ... }
    *   object SomeImplicits extends SomeImplicits
    *
    *   trait SomeRealRpc { ... }
    * }}}
    *
    * `RpcMacroInstances.materialize[SomeImplicits, SomeInstances, SomeRealRpc]` would generate:
    *
    * {{{
    *   new RpcMacroInstances[SomeImplicits, SomeInstances, SomeRealRpc] {
    *     def apply(implicits: SomeImplicits): SomeInstances[SomeRealRpc] = {
    *       import implicits._
    *       new SomeInstances[SomeRealRpc] {
    *         def asReal: AsReal[SomeRawRpc, SomeRealRpc] = AsReal.materializeForRpc
    *         def metadata: SomeMetadata[Real] = RpcMetadata.materializeForRpc
    *       }
    *     }
    *   }
    * }}}
    */
  implicit def materialize[Implicits, InstancesTrait[_], Real]: RpcMacroInstances[Implicits, InstancesTrait, Real] =
  macro macros.rpc.RpcMacros.macroInstances
}

/**
  * Wrap your implicit instance of [[AsReal]], [[AsRaw]], [[AsRawReal]] or RPC metadata (with companion that extends
  * [[RpcMetadataCompanion]] into `Fallback` in order to lower its implicit priority.
  * Useful when some implicit must be imported but we don't want it to get higher priority that imports normally
  * have over implicit scope (e.g. implicits from companion objects).
  *
  * NOTE: `Fallback` does not work for *all* typeclasses, only RPC-related ones (`AsReal`, `AsRaw`, etc).
  * You can make it work with your own typeclass, but you must define appropriate forwarder in its companion, e.g.
  * {{{
  *   trait FallbackAwareTC[T] { ... }
  *   object FallbackAwareTC {
  *     implicit def fromFallback[T](implicit f: Fallback[FallbackAwareTC[T]]): FallbackAwareTC[T] = f.value
  *   }
  * }}}
  */
case class Fallback[+T](value: T) extends AnyVal
