package com.avsystem.commons
package serialization

import com.avsystem.commons.serialization.GenCodec.WriteFailure

abstract class TransparentWrapperCompanion[R: GenCodec, T] {
  def apply(r: R): T
  def unapply(t: T): Option[R]

  implicit lazy val codec: GenCodec[T] = GenCodec.create[T](
    input => apply(GenCodec.read[R](input)),
    (output, value) => GenCodec.write[R](output, unapply(value)
      .getOrElse(throw new WriteFailure(s"failure unwrapping value from $value")))
  )
}

abstract class StringWrapperCompanion[T]
  extends TransparentWrapperCompanion[String, T]
