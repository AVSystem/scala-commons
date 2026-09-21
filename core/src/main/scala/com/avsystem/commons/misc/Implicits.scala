package com.avsystem.commons
package misc

import scala.annotation.nowarn

object Implicits {

  /** Similar to `implicitly` from standard library but implemented as a macro which materializes directly into the
    * implicit value (without being wrapped as implicit parameter of a dummy method like `implicitly`). Also, using
    * `infer` lets you have more detailed control over implicit-not-found compilation error messages through
    * [[ImplicitNotFound]].
    */
  @deprecated("Use native implicitly instead", "2.29.0")
  def infer[T]: T = macro macros.misc.MiscMacros.infer[T]
  @deprecated("Use native implicitly instead", "2.29.0")
  def infer[T](clue: String): T = macro macros.misc.MiscMacros.clueInfer[T]
  @deprecated("Use native implicitly instead", "2.29.0")
  def inferNonMacro[T](clue: String): T = macro macros.misc.MiscMacros.inferNonMacro[T]
}

/** Extends the functionality of [[scala.annotation.implicitNotFound]] so that implicit-not-found error message is
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
@deprecated("Use native implicitNotFound instead", "2.29.0")
sealed trait ImplicitNotFound[T]

@nowarn("msg=deprecated")
object ImplicitNotFound {
  @deprecated("Use native implicitNotFound instead", "2.29.0")
  def apply[T](): ImplicitNotFound[T] = throw new NotImplementedError("ImplicitNotFound.apply")

  @deprecated("Use native implicitNotFound instead", "2.29.0")
  implicit def dummy[T]: ImplicitNotFound[T] = apply()
}
