package com.avsystem.commons
package jiop

import com.avsystem.commons.meta.OptionLike

import java.util as ju

trait JOptionalUtils {

  type JOptional[T] = ju.Optional[T]
  type JOptionalDouble = ju.OptionalDouble
  type JOptionalInt = ju.OptionalInt
  type JOptionalLong = ju.OptionalLong

  extension [T](optional: JOptional[T]) {
    def toOption: Option[T] =
      if (optional.isPresent) Some(optional.get) else None

    def toOpt: Opt[T] =
      if (optional.isPresent) Opt(optional.get) else Opt.Empty

    def asScala: Option[T] = toOption
  }

  extension (optional: JOptionalDouble) {
    def toOption: Option[Double] =
      if (optional.isPresent) Some(optional.getAsDouble) else None

    def toOpt: Opt[Double] =
      if (optional.isPresent) Opt(optional.getAsDouble) else Opt.Empty

    def asScala: Option[Double] = toOption
  }

  extension (optional: JOptionalInt) {
    def toOption: Option[Int] =
      if (optional.isPresent) Some(optional.getAsInt) else None

    def toOpt: Opt[Int] =
      if (optional.isPresent) Opt(optional.getAsInt) else Opt.Empty

    def asScala: Option[Int] = toOption
  }
  extension (optional: JOptionalLong) {
    def toOption: Option[Long] =
      if (optional.isPresent) Some(optional.getAsLong) else None

    def toOpt: Opt[Long] =
      if (optional.isPresent) Opt(optional.getAsLong) else Opt.Empty

    def asScala: Option[Long] = toOption
  }

  extension (option: Option[Double]) {
    def toJOptionalDouble: JOptionalDouble =
      if (option.isDefined) ju.OptionalDouble.of(option.get) else ju.OptionalDouble.empty()

    def asJavaDouble: JOptionalDouble = toJOptionalDouble
  }

  extension (option: Option[Int]) {
    def toJOptionalInt: JOptionalInt =
      if (option.isDefined) ju.OptionalInt.of(option.get) else ju.OptionalInt.empty()

    def asJavaInt: JOptionalInt = toJOptionalInt
  }

  extension (option: Option[Long]) {
    def toJOptionalLong: JOptionalLong =
      if (option.isDefined) ju.OptionalLong.of(option.get) else ju.OptionalLong.empty()

    def asJavaLong: JOptionalLong = toJOptionalLong
  }

  object JOptional {
    def apply[T](nullable: T | Null): JOptional[T] = ju.Optional.ofNullable(nullable)

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

  extension [O[_], T](opt: O[T])(using optionLike: OptionLike.Aux[O[T], T]) {
    def toJOptional: JOptional[T] =
      if (optionLike.isDefined(opt)) ju.Optional.of(optionLike.get(opt)) else ju.Optional.empty()
    def asJava: JOptional[T] = toJOptional
  }
}

object JOptionalUtils extends JOptionalUtils
