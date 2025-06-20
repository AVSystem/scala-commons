package com.avsystem.commons
package opaque

import com.avsystem.commons.opaque.Opaque.Hidden

/** A trait for creating opaque types that are completely distinct from their underlying type.
 *
 * Opaque types created with this trait will have the same runtime representation as their
 * underlying type but will be treated as completely different types by the compiler.
 * This provides type safety without runtime overhead.
 *
 * @tparam From The underlying type that this opaque type wraps
 */
trait Opaque[From] extends BaseOpaque[From] {

  /** The opaque type, defined as a Hidden type that combines the underlying type with the Tag */
  final type Type = Hidden[From, Tag]
}

/** Companion object for the Opaque trait, containing helper types and implementations */
object Opaque {

  /** A type that represents an opaque wrapper around a value of type From, tagged with Tag.
   * This type is used to create distinct types that have no relationship to their underlying type.
   *
   * @tparam From The underlying type
   * @tparam Tag The tag type used to make this type unique
   */
  type Hidden[From, Tag]

  /** Provides a ClassTag for Hidden types based on the ClassTag of the underlying type.
   * This allows Hidden types to be used in contexts that require runtime type information.
   *
   * @param base The ClassTag for the underlying type
   * @return A ClassTag for the Hidden type
   */
  @inline implicit def classTag[From, Tag](implicit base: ClassTag[From]): ClassTag[Hidden[From, Tag]] = ClassTag(base.runtimeClass)

  /** A default implementation of Opaque that provides a simple apply method.
   * This is the most common way to create opaque types.
   *
   * @tparam From The underlying type that this opaque type wraps
   */
  trait Default[From] extends Opaque[From] {
    /** Creates an instance of the opaque type by simply wrapping the underlying value.
     *
     * @param value The value to wrap
     * @return The wrapped value as the opaque type
     */
    override final def apply(value: From): Type = wrap(value)
  }
}
