package com.avsystem.commons
package mongo

import com.avsystem.commons.serialization.RawRef.Field
import com.avsystem.commons.serialization.{GenCodec, RawRef}

case class BsonRef[T](path: String, codec: GenCodec[T])
object BsonRef {
  val BsonKeySeparator = "."

  trait Creator[S] {
    def ref[T](fun: S => T): BsonRef[T] = macro macros.serialization.BsonRefMacros.bsonRef[S]
  }

  def apply[T](rawRef: RawRef)(implicit codec: GenCodec[T]): BsonRef[T] = {
    val path = rawRef.normalize.map {
      case Field(name) => KeyEscaper.escape(name)
    }.mkString(BsonKeySeparator)

    BsonRef(path, codec)
  }
}
