package com.avsystem.commons
package misc

object Implicits extends ImplicitsMacros

/**
 * Extends the functionality of [[scala.annotation.implicitNotFound]] so that implicit-not-found error message is
 * itself resolved using implicit search. This mechanism is used by [[Implicits.infer[T]:T*]] and macro engines that
 * use it.
 *
 * Example: we have a wrapper class `Box[T]` and we want a custom error message when `GenCodec[Box[T]]` for some type
 * `T` is not found:
 *
 * {{{
 *   trait Box[T]
 *   object Box {
 *     implicit def boxCodec[T: GenCodec]: GenCodec[Box[T]] = ...
 *
 *     @implicitNotFound("GenCodec for Box[$${T}] not found. This is most likely caused by lack of GenCodec[$${T}]")
 *     implicit def boxCodecNotFound[T]: ImplicitNotFound[GenCodec[Box[T]]] = ImplicitNotFound()
 *   }
 * }}}
 *
 * It is also possible to compose error message for one type from error messages for other types. The example above
 * could reuse the implicit-not-found message for `GenCodec[T]` when building the message for `GenCodec[Box[T]]`:
 *
 * {{{
 *   @implicitNotFound("GenCodec for Box[$${T}] not found, because:\n#{forUnboxed}")
 *   implicit def boxCodecNotFound[T](
 *     implicit forUnboxed: ImplicitNotFound[GenCodec[T]]
 *   ): ImplicitNotFound[GenCodec[Box[T]]] = ImplicitNotFound()
 * }}}
 */

sealed trait ImplicitNotFound[T]
object ImplicitNotFound {
  def apply[T](): ImplicitNotFound[T] = throw new NotImplementedError("ImplicitNotFound.apply")

  implicit def dummy[T]: ImplicitNotFound[T] = apply()
}
