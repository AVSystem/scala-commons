package com.avsystem.commons
package serialization

/**
  * Convenience abstract class for companion objects of types that have a [[GenCodec]].
  * Unfortunately, due to compiler and language limitations this only works for non-generic case classes
  * without default constructor arguments.
  */
abstract class HasGenCodec[T](implicit macroCodec: MacroCodec[T]) {
  /**
    * Use this constructor and pass `GenCodec.materialize` explicitly if you're getting the
    * "super constructor cannot be passed a self reference unless parameter is declared by-name" error.
    *
    * {{{
    *   case class Stuff(int: Int = 42)
    *   object Stuff extends HasGenCodec[Stuff](GenCodec.materialize)
    * }}}
    */
  def this(codec: => GenCodec[T]) = this()(MacroCodec(codec))

  implicit val codec: GenCodec[T] = macroCodec.codec
}

case class MacroCodec[T](codec: GenCodec[T]) extends AnyVal
object MacroCodec {
  implicit def materialize[T]: MacroCodec[T] = macro macros.serialization.GenCodecMacros.materializeMacroCodec[T]
}
