package com.avsystem.commons
package jiop

import com.avsystem.commons.meta.OptionLike

import java.{util => ju}

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

    def asScala: Option[T] = optional.toOption
  }

  extension (optional: JOptionalDouble) {
    def toOption: Option[Double] =
      if (optional.isPresent) Some(optional.getAsDouble) else None

    def toOpt: Opt[Double] =
      if (optional.isPresent) Opt(optional.getAsDouble) else Opt.Empty

    def asScala: Option[Double] = optional.toOption
  }

  extension (optional: JOptionalInt) {
    def toOption: Option[Int] =
      if (optional.isPresent) Some(optional.getAsInt) else None

    def toOpt: Opt[Int] =
      if (optional.isPresent) Opt(optional.getAsInt) else Opt.Empty

    def asScala: Option[Int] = optional.toOption
  }

  extension (optional: JOptionalLong) {
    def toOption: Option[Long] =
      if (optional.isPresent) Some(optional.getAsLong) else None

    def toOpt: Opt[Long] =
      if (optional.isPresent) Opt(optional.getAsLong) else Opt.Empty

    def asScala: Option[Long] = optional.toOption
  }

  extension (option: Option[Double]) {
    def toJOptionalDouble: JOptionalDouble =
      if (option.isDefined) ju.OptionalDouble.of(option.get) else ju.OptionalDouble.empty()

    def asJavaDouble: JOptionalDouble = option.toJOptionalDouble
  }

  extension (option: Option[Int]) {
    def toJOptionalInt: JOptionalInt =
      if (option.isDefined) ju.OptionalInt.of(option.get) else ju.OptionalInt.empty()

    def asJavaInt: JOptionalInt = option.toJOptionalInt
  }

  extension (option: Option[Long]) {
    def toJOptionalLong: JOptionalLong =
      if (option.isDefined) ju.OptionalLong.of(option.get) else ju.OptionalLong.empty()

    def asJavaLong: JOptionalLong = option.toJOptionalLong
  }

  // Single generic extension for any option-like wrapper (Option, Opt, NOpt, OptArg).
  // Carries `using OptionLike.Aux[O[T], T]` so that resolution only kicks in for true
  // option-like receivers, avoiding clash with `scala.collection.convert.AsJavaExtensions.asJava`
  // on `Seq`/`Iterable`.
  extension [O[_], T](opt: O[T])(using optionLike: OptionLike.Aux[O[T], T]) {
    /** Note that in scala Some(null) is valid value. It will throw an exception in such case, because java Optional is
      * not able to hold null
      */
    def toJOptional: JOptional[T] =
      if (optionLike.isDefined(opt)) ju.Optional.of(optionLike.get(opt)) else ju.Optional.empty()
    def asJava: JOptional[T] = opt.toJOptional
  }

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

object JOptionalUtils extends JOptionalUtils
