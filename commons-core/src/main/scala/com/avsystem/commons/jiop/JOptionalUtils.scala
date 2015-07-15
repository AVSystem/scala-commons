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

  @inline implicit def optional2AsScala[T](optional: JOptional[T]): optional2AsScala[T] =
    new optional2AsScala(optional)

  @inline implicit def optionalDouble2AsScala(optional: JOptionalDouble): optionalDouble2AsScala =
    new optionalDouble2AsScala(optional)

  @inline implicit def optionalInt2AsScala(optional: JOptionalInt): optionalInt2AsScala =
    new optionalInt2AsScala(optional)

  @inline implicit def optionalLong2AsScala(optional: JOptionalLong): optionalLong2AsScala =
    new optionalLong2AsScala(optional)
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

}
