package com.avsystem.commons
package meta

/**
  * Intermediate factory that creates an `Instances` trait based on provided `Implicits`.
  * Normally, this factory is used as implicit constructor parameter of base classes for companion objects
  * of RPC traits (e.g. [[com.avsystem.commons.rest.DefaultRestApiCompanion DefaultRestApiCompanion]]).
  * This all serves to reduce boilerplate associated with RPC trait companion declarations and makes RPC trait
  * definitions as concise as possible. It also lets the programmer easily inject additional implicits into
  * macro-materialization of RPC-related typeclasses (`AsReal`, `AsRaw`, metadata, etc.).
  *
  * `Instances` is a trait that aggregates multiple macro materialized typeclass instances.
  * There is no fixed interface for `Instances`, its members are inspected by `materialize` macro and implemented
  * automatically. `Instances` must contain only parameterless abstract methods. Return type of each method
  * must have a companion object which contains `materialize` macro. That macro will be used to implement
  * that method.
  *
  * Example of `Instances`: [[com.avsystem.commons.rest.ClientInstances ClientInstances]]
  *
  * The `Implicits` type is typically a trait with a collection of implicit definitions whose companion object
  * implements that trait, e.g. [[com.avsystem.commons.rest.DefaultRestImplicits DefaultRestImplicits]].
  * When the macro implements `apply` method of `RpcMacroInstances` contents of `Implicits` are imported into the
  * body of `apply` and visible further by macros that materialize `InstancesTrait`.
  *
  * If `MacroInstances` is accepted as implicit super constructor parameter of a companion object
  * (which is the typical situation) then `this` reference should be passed as `companion`.
  * This is in order to work around https://github.com/scala/bug/issues/7666
  */
trait MacroInstances[Implicits, Instances] {
  def apply(implicits: Implicits, companion: Any): Instances
}

object MacroInstances {
  /**
    * Materializes an instance of `MacroInstances[Implicits, Instances]`. This macro should not be
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
    * `MacroInstances.materialize[SomeImplicits, SomeInstances[SomeRealRpc]]` would generate:
    *
    * {{{
    *   new MacroInstances[SomeImplicits, SomeInstances[SomeRealRpc]] {
    *     def apply(implicits: SomeImplicits): SomeInstances[SomeRealRpc] = {
    *       import implicits._
    *       new SomeInstances[SomeRealRpc] {
    *         def asReal: AsReal[SomeRawRpc, SomeRealRpc] = AsReal.materialize
    *         def metadata: SomeMetadata[Real] = RpcMetadata.materialize
    *       }
    *     }
    *   }
    * }}}
    */
  implicit def materialize[Implicits, Instances]: MacroInstances[Implicits, Instances] =
  macro macros.misc.MiscMacros.macroInstances
}