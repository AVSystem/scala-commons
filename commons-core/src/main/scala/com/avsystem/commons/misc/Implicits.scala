package com.avsystem.commons
package misc

object Implicits {
  def infer[T](clue: String): T = macro macros.misc.MiscMacros.infer[T]
  def inferNonMacro[T](clue: String): T = macro macros.misc.MiscMacros.inferNonMacro[T]
}

/**
  * Extends the functionality of [[scala.annotation.implicitNotFound]] so that implicit-not-found error message
  * is itself resolved using implicit search. This mechanism is used by [[Implicits.infer]] and macro engines
  * that use it.
  *
  * Example: we have a wrapper class `Box[T]` and we want a custom error message when `GenCodec[Box[T]]` for some type
  * `T` is not found:
  *
  * {{{
  *   trait Box[T]
  *   object Box {
  *     implicit def boxCodecFromClassTag[T: ClassTag]: GenCodec[Box[T]] = ...
  *
  *     @implicitNotFound("GenCodec for Box[$${T}] not found. This is most likely caused by lack of ClassTag[$${T}]")
  *     implicit def boxCodecNotFound[T]: ImplicitNotFound[GenCodec[Box[T]]] = ImplicitNotFound()
  *   }
  * }}}
  */
sealed trait ImplicitNotFound[T]
object ImplicitNotFound {
  def apply[T](): ImplicitNotFound[T] = throw new NotImplementedError("ImplicitNotFound.apply")
}
