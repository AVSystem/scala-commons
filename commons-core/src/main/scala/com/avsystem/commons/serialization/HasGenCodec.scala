package com.avsystem.commons
package serialization

import com.avsystem.commons.misc.MacroGenerated

/**
  * Convenience abstract class for companion objects of types that have a [[GenCodec]].
  */
abstract class HasGenCodec[T](implicit macroCodec: MacroGenerated[GenCodec[T]]) {
  /**
    * Use this constructor and pass `GenCodec.materialize` explicitly if you're getting the
    * "super constructor cannot be passed a self reference unless parameter is declared by-name" error.
    *
    * {{{
    *   case class Stuff(int: Int = 42)
    *   object Stuff extends HasGenCodec[Stuff](GenCodec.materialize)
    * }}}
    */
  def this(codec: => GenCodec[T]) = this()(MacroGenerated(codec))

  implicit val codec: GenCodec[T] = macroCodec.forCompanion(this)
}
