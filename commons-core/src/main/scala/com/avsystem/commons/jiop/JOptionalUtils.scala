package com.avsystem.commons
package jiop

import java.{util => ju}

/**
  * Author: ghik
  * Created: 14/07/15.
  */
trait JOptionalUtils {

  import JOptionalUtils._

  type JOptional[T] = ju.Optional[T]
  type JOptionalDouble = ju.OptionalDouble
  type JOptionalInt = ju.OptionalInt
  type JOptionalLong = ju.OptionalLong

  implicit def optional2AsScala[T](optional: JOptional[T]): optional2AsScala[T] =
    new optional2AsScala(optional)

  implicit def optionalDouble2AsScala(optional: JOptionalDouble): optionalDouble2AsScala =
    new optionalDouble2AsScala(optional)

  implicit def optionalInt2AsScala(optional: JOptionalInt): optionalInt2AsScala =
    new optionalInt2AsScala(optional)

  implicit def optionalLong2AsScala(optional: JOptionalLong): optionalLong2AsScala =
    new optionalLong2AsScala(optional)

  implicit def option2AsJava[T](option: Option[T]): option2AsJava[T] =
    new option2AsJava(option)

  implicit def option2AsJavaDouble(option: Option[Double]): option2AsJavaDouble =
    new option2AsJavaDouble(option)

  implicit def option2AsJavaInt(option: Option[Int]): option2AsJavaInt =
    new option2AsJavaInt(option)

  implicit def option2AsJavaLong(option: Option[Long]): option2AsJavaLong =
    new option2AsJavaLong(option)
}

object JOptional {

  import JavaInterop._

  def apply[T](nullable: T): JOptional[T] = ju.Optional.ofNullable(nullable)

  def empty[T]: JOptional[T] = ju.Optional.empty[T]()

  def none[T]: JOptional[T] = empty
}

object JOptionalDouble {

  import JavaInterop._

  def apply(value: Double): JOptionalDouble = ju.OptionalDouble.of(value)

  def empty: JOptionalDouble = ju.OptionalDouble.empty()

  def none: JOptionalDouble = empty
}

object JOptionalInt {

  import JavaInterop._

  def apply(value: Int): JOptionalInt = ju.OptionalInt.of(value)

  def empty: JOptionalInt = ju.OptionalInt.empty()

  def none: JOptionalInt = empty
}

object JOptionalLong {

  import JavaInterop._

  def apply(value: Long): JOptionalLong = ju.OptionalLong.of(value)

  def empty: JOptionalLong = ju.OptionalLong.empty()

  def none: JOptionalLong = empty
}

object JOptionalUtils {

  import JavaInterop._

  final class optional2AsScala[T](private val optional: JOptional[T]) extends AnyVal {
    def asScala: Option[T] =
      if (optional.isPresent) Some(optional.get) else None
  }

  final class optionalDouble2AsScala(private val optional: JOptionalDouble) extends AnyVal {
    def asScala: Option[Double] =
      if (optional.isPresent) Some(optional.getAsDouble) else None
  }

  final class optionalInt2AsScala(private val optional: JOptionalInt) extends AnyVal {
    def asScala: Option[Int] =
      if (optional.isPresent) Some(optional.getAsInt) else None
  }

  final class optionalLong2AsScala(private val optional: JOptionalLong) extends AnyVal {
    def asScala: Option[Long] =
      if (optional.isPresent) Some(optional.getAsLong) else None
  }

  // Note that in scala Some(null) is valid value. It will be translated to Optional.empty, because java Optional
  // is not able to hold null
  final class option2AsJava[T](private val option: Option[T]) extends AnyVal {
    def asJava: JOptional[T] =
      if (option.isDefined) ju.Optional.ofNullable(option.get) else ju.Optional.empty()
  }

  final class option2AsJavaDouble(private val option: Option[Double]) extends AnyVal {
    def asJavaDouble: JOptionalDouble =
      if (option.isDefined) ju.OptionalDouble.of(option.get) else ju.OptionalDouble.empty()
  }

  final class option2AsJavaInt(private val option: Option[Int]) extends AnyVal {
    def asJavaInt: JOptionalInt =
      if (option.isDefined) ju.OptionalInt.of(option.get) else ju.OptionalInt.empty()
  }

  final class option2AsJavaLong(private val option: Option[Long]) extends AnyVal {
    def asJavaLong: JOptionalLong =
      if (option.isDefined) ju.OptionalLong.of(option.get) else ju.OptionalLong.empty()
  }

}
