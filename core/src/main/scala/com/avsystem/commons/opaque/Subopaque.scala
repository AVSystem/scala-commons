package com.avsystem.commons
package opaque

/** A trait for creating opaque types that are subtypes of their underlying type.
 *
 * Opaque types created with this trait will have the same runtime representation as their
 * underlying type and will be treated as subtypes of their underlying type by the compiler.
 * This provides a way to create more specific types that can be used where the underlying type is expected.
 *
 * @tparam From The underlying type that this opaque type wraps and extends
 */
trait Subopaque[From] extends BaseOpaque[From] {

  /** The opaque type, defined as an intersection type between the underlying type and the Tag */
  final type Type = From & Tag

  /** Creates an instance of the opaque type from a value of the underlying type
   *
   * @param value The value to wrap in the opaque type
   * @return The wrapped value as the opaque type
   */
  def apply(value: From): Type
}

/** Companion object for the Subopaque trait, containing helper implementations */
object Subopaque {

  /** A default implementation of Subopaque that provides a simple apply method.
   * This is the most common way to create subopaque types.
   *
   * @tparam From The underlying type that this opaque type wraps and extends
   */
  trait Default[From] extends Subopaque[From] {
    /** Creates an instance of the opaque type by simply wrapping the underlying value.
     *
     * @param value The value to wrap
     * @return The wrapped value as the opaque type
     */
    override final def apply(value: From): Type = wrap(value)
  }
}
