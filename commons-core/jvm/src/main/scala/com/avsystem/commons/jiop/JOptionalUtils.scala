package com.avsystem.commons
package jiop

import java.{util => ju}

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


  object JOptional {
    def apply[T](nullable: T): JOptional[T] = ju.Optional.ofNullable(nullable)

    def empty[T]: JOptional[T] = ju.Optional.empty[T]()
  }

  object JOptionalDouble {
    def apply(value: Double): JOptionalDouble = ju.OptionalDouble.of(value)

    def empty: JOptionalDouble = ju.OptionalDouble.empty()
  }

  object JOptionalInt {
    def apply(value: Int): JOptionalInt = ju.OptionalInt.of(value)

    def empty: JOptionalInt = ju.OptionalInt.empty()
  }

  object JOptionalLong {
    def apply(value: Long): JOptionalLong = ju.OptionalLong.of(value)

    def empty: JOptionalLong = ju.OptionalLong.empty()
  }

}

object JOptionalUtils {

  final class optional2AsScala[T](private val optional: JOptional[T]) extends AnyVal {
    def toOption: Option[T] =
      if (optional.isPresent) Some(optional.get) else None

    def toOpt: Opt[T] =
      if (optional.isPresent) Opt(optional.get) else Opt.Empty

    def asScala: Option[T] = toOption
  }

  final class optionalDouble2AsScala(private val optional: JOptionalDouble) extends AnyVal {
    def toOption: Option[Double] =
      if (optional.isPresent) Some(optional.getAsDouble) else None

    def toOpt: Opt[Double] =
      if (optional.isPresent) Opt(optional.getAsDouble) else Opt.Empty

    def asScala: Option[Double] = toOption
  }

  final class optionalInt2AsScala(private val optional: JOptionalInt) extends AnyVal {
    def toOption: Option[Int] =
      if (optional.isPresent) Some(optional.getAsInt) else None

    def toOpt: Opt[Int] =
      if (optional.isPresent) Opt(optional.getAsInt) else Opt.Empty

    def asScala: Option[Int] = toOption
  }

  final class optionalLong2AsScala(private val optional: JOptionalLong) extends AnyVal {
    def toOption: Option[Long] =
      if (optional.isPresent) Some(optional.getAsLong) else None

    def toOpt: Opt[Long] =
      if (optional.isPresent) Opt(optional.getAsLong) else Opt.Empty

    def asScala: Option[Long] = toOption
  }

  final class option2AsJava[T](private val option: Option[T]) extends AnyVal {
    /**
      * Note that in scala Some(null) is valid value. It will throw an exception in such case, because java Optional
      * is not able to hold null
      */
    def toJOptional: JOptional[T] =
      if (option.isDefined) ju.Optional.of(option.get) else ju.Optional.empty()

    def asJava: JOptional[T] = toJOptional
  }

  final class option2AsJavaDouble(private val option: Option[Double]) extends AnyVal {
    def toJOptionalDouble: JOptionalDouble =
      if (option.isDefined) ju.OptionalDouble.of(option.get) else ju.OptionalDouble.empty()

    def asJavaDouble: JOptionalDouble = toJOptionalDouble
  }

  final class option2AsJavaInt(private val option: Option[Int]) extends AnyVal {
    def toJOptionalInt: JOptionalInt =
      if (option.isDefined) ju.OptionalInt.of(option.get) else ju.OptionalInt.empty()

    def asJavaInt: JOptionalInt = toJOptionalInt
  }

  final class option2AsJavaLong(private val option: Option[Long]) extends AnyVal {
    def toJOptionalLong: JOptionalLong =
      if (option.isDefined) ju.OptionalLong.of(option.get) else ju.OptionalLong.empty()

    def asJavaLong: JOptionalLong = toJOptionalLong
  }

}
