package com.avsystem.commons
package opaque

import com.avsystem.commons.opaque.Castable.<:>
import com.avsystem.commons.serialization.{GenCodec, GenKeyCodec}

/** Base trait for opaque type definitions.
  *
  * This trait provides the foundation for creating opaque types in Scala 2.
  * Opaque types are types that have the same runtime representation as their underlying type
  * but are treated as distinct types by the compiler.
  *
  * @tparam From The underlying type that this opaque type wraps
  */
private[opaque] trait BaseOpaque[From] extends Castable.Ops {
  /** A marker trait used to create a unique type for this opaque type */
  trait Tag

  /** The opaque type itself, defined as a type member */
  type Type

  /** Creates an instance of the opaque type from a value of the underlying type
    *
    * @param value The value to wrap in the opaque type
    * @return The wrapped value as the opaque type
    */
  def apply(value: From): Type

  /** Implicit evidence that values of type `From` can be cast to `Type` and vice versa */
  implicit protected final val castable: From <:> Type = new Castable[From, Type]

  /** Provides automatic codec derivation for the opaque type based on the codec for the underlying type */
  implicit final def transparentCodec(implicit fromCodec: GenCodec[From]): GenCodec[Type] = wrapF(fromCodec)

  /** Provides automatic key codec derivation for the opaque type based on the key codec for the underlying type */
  implicit final def transparentKeyCodec(implicit fromKeyCodec: GenKeyCodec[From]): GenKeyCodec[Type] = wrapF(fromKeyCodec)
}
