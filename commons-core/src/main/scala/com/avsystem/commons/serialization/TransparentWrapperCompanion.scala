package com.avsystem.commons
package serialization

import com.avsystem.commons.serialization.GenCodec.ReadFailure

abstract class TransparentWrapperCompanion[R: GenCodec, T] {
  def apply(r: R): T
  def unapply(t: T): Option[R]

  implicit lazy val codec: GenCodec[T] = GenCodec.createNullSafe[T](
    input => apply(GenCodec.read[R](input)),
    (output, value) => GenCodec.write[R](output, unapply(value).getOrElse(throw new ReadFailure("unwrapping failed"))),
    allowNull = true
  )
}

abstract class StringWrapperCompanion[T]
  extends TransparentWrapperCompanion[String, T]
