package com.avsystem.commons
package misc

/**
  * Author: ghik
  * Created: 11/12/15.
  */
object SealedUtils {
  def caseObjectsFor[T]: List[T] = macro macros.misc.SealedMacros.caseObjectsFor[T]
}

/**
  * Base trait for companion objects of sealed traits that serve as enums, i.e. their only values are case objects.
  * For example:
  *
  * {{{
  *   sealed trait SomeEnum
  *   object SomeEnum extends SealedEnumCompanion[SomeEnum] {
  *     case object FirstValue extends SomeEnum
  *     case object SecondValue extends SomeEnum
  *     case object ThirdValue extends SomeEnum
  *
  *     // it's important to give explicit type here
  *     val values: List[SomeEnum] = caseObjects
  *   }
  * }}}
  */
trait SealedEnumCompanion[T] {
  /**
    * Holds a list of all case objects of a sealed trait or class `T`. This must be implemented separately
    * for every sealed enum, but can be implemented simply by using the [[caseObjects]] macro.
    * It's important to *always* state the type of `values` explicitly, as a workaround for SI-7046. For example:
    *
    * {{{
    *   val values: List[MyEnum] = caseObjects
    * }}}
    */
  val values: List[T]

  protected def caseObjects: List[T] = macro macros.misc.SealedMacros.caseObjectsFor[T]
}
