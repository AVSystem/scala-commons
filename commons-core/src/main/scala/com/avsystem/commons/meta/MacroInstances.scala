package com.avsystem.commons
package meta

import scala.annotation.StaticAnnotation

/**
  * Intermediate factory that creates an `Instances` trait based on provided `Implicits`.
  * Normally, this factory is used as implicit constructor parameter of base classes for companion objects
  * of RPC traits (e.g. `com.avsystem.commons.rest.DefaultRestApiCompanion`)
  * or ADTs (e.g. `com.avsystem.commons.rest.RestDataCompanion`).
  * This all serves to reduce boilerplate associated with companion declarations and makes RPC trait or ADT
  * definitions as concise as possible. It also lets the programmer easily inject additional implicits into
  * macro-materialization of typeclasses aggregated by `Instances` trait.
  *
  * `Instances` is a trait that aggregates multiple macro materialized typeclass instances.
  * There is no fixed interface for `Instances`, its abstract methods are inspected by
  * `MacroInstances.materialize` macro and implemented by inserting appropriate materializer macro invocation
  * for each typeclass. By default it will be assumed that the typeclass has a macro named `materialize` in its
  * companion object so each method will get `<ResultTypeCompanion>.materialize` as its implementation.
  * This may be overridden with [[com.avsystem.commons.meta.MacroInstances.materializeWith materializeWith]]
  * annotation used on the method. This way you can specify both the object which contains the materializer macro
  * and its name.
  *
  * Additionally, all non-implicit parameters of each method in `Instances` trait will be automatically passed to
  * the materializer macro. See [[com.avsystem.commons.serialization.HasGenCodecFromAU HasGenCodecFromAU]] for
  * an example of this mechanism.
  *
  * Example of `Instances`: `com.avsystem.commons.rest.ClientInstances`
  *
  * The `Implicits` type specifies additional implicits that will be automatically imported into macro materialization.
  * `Implicits` is usually a singleton type of an object which contains these implicits. It may also be a tuple -
  * contents of each tuple component will be imported independently. This way you can combine multiple sources
  * of additional implicits. If you don't want to import any additional implicits, simply use `Unit`.
  *
  * If `MacroInstances` is accepted as implicit super constructor parameter of a companion object
  * (which is the typical situation) then `this` reference should be passed as `companion`.
  * This is in order to work around https://github.com/scala/bug/issues/7666.
  * Actual typeclass instances aggregated by `Instances` trait should be extracted into `implicit lazy val`
  * definitions in the companion base class. See e.g. `com.avsystem.commons.rest.RestDataCompanion`
  * for an example of how it's done.
  */
trait MacroInstances[Implicits, Instances] {
  def apply(implicits: Implicits, companion: Any): Instances
}

object MacroInstances {
  /**
    * Annotation which may be applied on methods of `Implicits` trait in [[MacroInstances]] to instruct
    * [[MacroInstances.materialize]] macro how to implement these methods.
    */
  final class materializeWith(prefix: Any, materializer: String = "materialize") extends StaticAnnotation

  /**
    * Materializes an instance of `MacroInstances[Implicits, Instances]`. This macro should not be
    * invoked directly, it should only be used to materialize implicit parameters of RPC companion base classes,
    * e.g. `com.avsystem.commons.rest.DefaultRestApiCompanion`.
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