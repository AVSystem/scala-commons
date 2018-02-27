package com.avsystem.commons
package misc

import com.avsystem.commons.annotation.explicitGenerics
import com.avsystem.commons.serialization.{GenCodec, GenKeyCodec}

object SealedUtils {
  /**
    * A macro which reifies a list of all case objects of a sealed trait or class `T`.
    * WARNING: the order of case objects in the resulting list is guaranteed to be consistent with
    * declaration order ONLY for enums extending [[OrderedEnum]]. Otherwise, the order may be arbitrary.
    */
  @explicitGenerics
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
  *     // it's important to explicitly specify the type so that `caseObjects` macro works properly
  *     val values: List[SomeEnum] = caseObjects
  *   }
  * }}}
  */
trait SealedEnumCompanion[T] {
  /**
    * Thanks to this implicit, [[SealedEnumCompanion]] and its subtraits can be used as typeclasses.
    */
  implicit def evidence: this.type = this

  /**
    * Holds a list of all case objects of a sealed trait or class `T`. This must be implemented separately
    * for every sealed enum, but can be implemented simply by using the [[caseObjects]] macro.
    * It's important to *always* state the type of `values` explicitly, as a workaround for SI-7046. For example:
    *
    * {{{
    *   val values: List[MyEnum] = caseObjects
    * }}}
    *
    * Also, be aware that [[caseObjects]] macro guarantees well-defined order of elements only for
    * [[com.avsystem.commons.misc.OrderedEnum OrderedEnum]].
    */
  val values: ISeq[T]

  /**
    * A macro which reifies a list of all case objects of the sealed trait or class `T`.
    * WARNING: the order of case objects in the resulting list is well defined only for enums that extend [[OrderedEnum]].
    * In such case, the order is consistent with declaration order in source file. However, if the enum is not an
    * [[OrderedEnum]], the order may be arbitrary.
    */
  protected def caseObjects: List[T] = macro macros.misc.SealedMacros.caseObjectsFor[T]
}

abstract class AbstractSealedEnumCompanion[T] extends SealedEnumCompanion[T]

/**
  * Base trait for enums implemented as sealed hierarchy with case objects where every enum value has distinct
  * textual representation (name).
  *
  * Typically, if a trait or class extends `NamedEnum`, its companion object extends [[NamedEnumCompanion]].
  * Enum values can then be looked up by name using [[NamedEnumCompanion.byName]].
  */
trait NamedEnum extends Serializable {
  /**
    * Used as a key for a map returned from `byName`. It is recommended to override this method uniquely
    * by each case object in the sealed hierarchy.
    */
  def name: String
  override def toString: String = name
}

/**
  * Base trait for companion objects of sealed traits that serve as named enums. `NamedEnumCompanion` is an
  * extension of [[SealedEnumCompanion]] which additionally requires that every enum value has distinct string
  * representation. Values can then be looked up by that representation using [[NamedEnumCompanion.byName]]
  *
  * Example:
  *
  * {{{
  *   sealed abstract class Color(val name: String) extends NamedEnum
  *   object Color extends NamedEnumCompanion[Color] {
  *     case object Red extends Color("red")
  *     case object Blue extends Color("blue")
  *     case object Green extends Color("green")
  *
  *     // it's important to explicitly specify the type so that `caseObjects` macro works properly
  *     val values: List[Color] = caseObjects
  *   }
  * }}}
  *
  * `NamedEnumCompanion` also automatically provides implicit typeclass instances for
  * [[com.avsystem.commons.serialization.GenKeyCodec GenKeyCodec]] and [[com.avsystem.commons.serialization.GenCodec GenCodec]].
  */
trait NamedEnumCompanion[T <: NamedEnum] extends SealedEnumCompanion[T] {
  /**
    * Returns a map from all case objects names to their instances.
    * Since `byName` uses [[caseObjects]] macro it does NOT guarantee an order of elements. It is also essential
    * to provide unique names for each case object in the sealed hierarchy to retrieve valid hierarchy.
    */
  lazy val byName: Map[String, T] = values.toMapBy(_.name)

  implicit lazy val keyCodec: GenKeyCodec[T] = GenKeyCodec.create(byName, _.name)
  implicit lazy val codec: GenCodec[T] = GenCodec.createNullable[T](
    input => byName(input.readString()),
    (output, value) => output.writeString(value.name)
  )
}

/**
  * Trait to be extended by enums whose values are ordered by declaration order. Ordering is derived from
  * [[SourceInfo]] object, which is typically accepted as an implicit, e.g.
  *
  * {{{
  *   sealed abstract class MyOrderedEnum(implicit val sourceInfo: SourceInfo) extends OrderedEnum
  *   object MyOrderedEnum {
  *     case object First extends MyOrderedEnum
  *     case object Second extends MyOrderedEnum
  *     case object Third extends MyOrderedEnum
  *
  *     val values: List[MyOrderedEnum] = caseObjects
  *   }
  * }}}
  *
  * In the example above, `values` is guaranteed to return `First`, `Second` and `Third` objects in exactly that order.
  */
trait OrderedEnum {
  def sourceInfo: SourceInfo
}
object OrderedEnum {
  private object reusableOrdering extends Ordering[OrderedEnum] {
    def compare(x: OrderedEnum, y: OrderedEnum) = Integer.compare(x.sourceInfo.offset, y.sourceInfo.offset)
  }
  implicit def ordering[T <: OrderedEnum]: Ordering[T] =
    reusableOrdering.asInstanceOf[Ordering[T]]
}

abstract class AbstractNamedEnumCompanion[T <: NamedEnum]
  extends AbstractSealedEnumCompanion[T] with NamedEnumCompanion[T]
