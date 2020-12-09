package com.avsystem.commons
package misc

import scala.annotation.implicitNotFound

/**
  * Base trait for `val`-based enums, i.e. enums implemented as a single class with companion object keeping
  * enum values as instances of the enum class in `final val` fields. This is an alternative way of implementing
  * enums as compared to traditional Scala approach of using a sealed hierarchy with objects representing enum values.
  *
  * Advantages of value based enums over object based enums include:
  * <ul>
  * <li>Much less classes generated, which in particular contributes to much less JavaScript output in ScalaJS.
  * This may also speed up compilation.</li>
  * <li>No need to explicitly implement `values` in enum's companion object as it is necessary when using
  * [[SealedEnumCompanion]] and [[NamedEnumCompanion]]</li>
  * <li>It is possible to define all enum values in a single line of code (assuming they don't take parameters)</li>
  * </ul>
  * Disadvantages of value based enums over object based enums include:
  * <ul>
  * <li>Every object can have its own separate public API, values cannot (although you can have a separate
  * anonymous class for every value)</li>
  * <li>Scala compiler does not perform exhaustive checking for pattern matching enum values - this is however
  * provided separately by `commons-analyzer` compiler plugin.</li>
  * </ul>
  *
  * Enum classes must have a companion object which extends [[ValueEnumCompanion]] (prefer using
  * [[AbstractValueEnumCompanion]] where possible). Every enum constant must be declared as a `final val` in the
  * companion object and must have the `Value` type explicitly ascribed (which is just a type alias for enum class itself).
  * The enum class itself must take an implicit [[EnumCtx]] argument which provides information about
  * enum ordinal and name as well as makes sure that enum value is registered in the companion object.
  * If possible, you should always extend [[AbstractValueEnum]] instead of mixing in this trait.
  * [[ValueEnum]] trait should only be mixed in directly when your enum class already has another superclass,
  * incompatible with [[AbstractValueEnum]].
  *
  * @example
  * {{{
  *   final class Weekday(implicit enumCtx: EnumCtx) extends AbstractValueEnum
  *   object Weekday extends AbstractValueEnumCompanion[Weekday] {
  *     final val Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday: Value = new Weekday
  *   }
  * }}}
  * Value based enums can take parameters:
  * {{{
  *   final class Status(val description: String)(implicit enumCtx: EnumCtx) extends AbstractValueEnum
  *   object Status extends AbstractValueEnumCompanion[Status] {
  *     final val Enabled: Value = new Status("Something is enabled and working")
  *     final val Disabled: Value = new Status("Something is disabled and not working")
  *   }
  * }}}
  */
trait ValueEnum extends NamedEnum {
  protected def enumCtx: EnumCtx

  enumCtx.register(this)

  /**
    * Enum value index, starting from 0. Reflects the order in which enum constants are declared in the
    * companion object of the enum class.
    */
  def ordinal: Int = enumCtx.ordinal

  /**
    * Name of the `final val` in enum companion object that this enum value is assigned to.
    */
  def name: String = enumCtx.valName
}

/**
  * Convenience abstract class implementing [[ValueEnum]]. For less generated code, faster compilation and
  * better binary compatibility it's better to extend this abstract class rather than [[ValueEnum]] trait directly.
  * See [[ValueEnum]] documentation for more information on value-based enums.
  */
abstract class AbstractValueEnum(protected implicit val enumCtx: EnumCtx) extends ValueEnum

@implicitNotFound("Value based enum must be assigned to a public, final, non-lazy val in its companion object " +
  "with explicit `Value` type ascribed, e.g. `final val Monday: Value = new Weekday")
sealed trait EnumCtx extends Any {
  def ordinal: Int
  def valName: String
  private[commons] def register(value: ValueEnum): Unit
}

/**
  * Base trait for companion objects of value based enums. See [[ValueEnum]] for more information.
  * NOTE: if possible, prefer using [[AbstractValueEnumCompanion]] instead of this trait directly.
  */
trait ValueEnumCompanion[T <: ValueEnum] extends NamedEnumCompanion[T] { companion =>
  type Value = T

  private[this] val registryBuilder = IIndexedSeq.newBuilder[T]
  private[this] var currentOrdinal: Int = 0
  private[this] var finished: Boolean = false
  private[this] var awaitingRegister: Boolean = false

  /**
    * Holds an indexed sequence of all enum values, ordered by their ordinal
    * (`values(i).ordinal` is always equal to `i`).
    */
  final lazy val values: IIndexedSeq[T] = synchronized {
    if (awaitingRegister) {
      throw new IllegalStateException(s"Cannot collect enum values - one of the created contexts didn't register a value yet")
    }
    finished = true
    registryBuilder.result()
  }

  implicit final val ordering: Ordering[T] = Ordering.by(_.ordinal)
  implicit final def ordered(value: T): Ordered[T] = Ordered.orderingToOrdered(value)

  private class Ctx(val valName: String, val ordinal: Int) extends EnumCtx {
    if (awaitingRegister) {
      throw new IllegalStateException(s"Cannot create new EnumCtx until the previous one registered a value")
    }
    awaitingRegister = true

    private[this] var registered = false

    override def register(value: ValueEnum): Unit = companion.synchronized {
      if (finished)
        throw new IllegalStateException(s"Enum values have already been collected - too late to register enum $value")
      else if (registered)
        throw new IllegalStateException("Cannot register using the same EnumCtx more than once")
      else {
        registryBuilder += value.asInstanceOf[T] //`enumValName` macro performs static checks that make this safe
        currentOrdinal += 1
        registered = true
        awaitingRegister = false
      }
    }
  }

  protected[this] final class ValName(val valName: String)

  protected[this] implicit def valName: ValName = macro macros.misc.MiscMacros.enumValName

  protected[this] implicit def enumCtx(implicit valName: ValName): EnumCtx =
    new Ctx(valName.valName, currentOrdinal)
}

/**
  * Convenience abstract class implementing [[ValueEnumCompanion]]. For less generated code, faster compilation and
  * better binary compatibility it's better to use this abstract class rather than [[ValueEnumCompanion]]
  * trait directly. See [[ValueEnum]] documentation for more information on value based enums.
  */
abstract class AbstractValueEnumCompanion[T <: ValueEnum]
  extends AbstractNamedEnumCompanion[T] with ValueEnumCompanion[T]
