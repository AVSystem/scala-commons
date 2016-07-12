package com.avsystem.commons
package misc

import com.avsystem.commons.serialization.GenKeyCodec

/**
  * Author: ghik
  * Created: 11/12/15.
  */
object SealedUtils {
  /**
    * A macro which reifies a list of all case objects of a sealed trait or class `T`.
    * WARNING: the order of case objects in the resulting list is arbitrary and is NOT guaranteed to be consistent with
    * declaration order.
    */
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
    *
    * Also, be aware that [[caseObjects]] macro does NOT guarantee any particular order of elements.
    */
  val values: List[T]

  /**
    * A macro which reifies a list of all case objects of the sealed trait or class `T`.
    * WARNING: the order of case objects in the resulting list is arbitrary and is NOT guaranteed to be consistent with
    * declaration order.
    */
  protected def caseObjects: List[T] = macro macros.misc.SealedMacros.caseObjectsFor[T]
}

trait NamedEnum extends Any {
  def name: String
  override def toString: String = name
}

trait NamedEnumCompanion[T <: NamedEnum] extends SealedEnumCompanion[T] {
  lazy val byName: Map[String, T] = values.iterator.map(v => (v.name, v)).toMap

  implicit lazy val keyCodec: GenKeyCodec[T] = GenKeyCodec.create(byName, _.name)
}
