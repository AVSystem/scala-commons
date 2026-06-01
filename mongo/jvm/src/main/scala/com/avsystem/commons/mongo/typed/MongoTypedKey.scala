package com.avsystem.commons
package mongo.typed

import com.avsystem.commons.misc.TypedKey
import com.avsystem.commons.misc.TypedMap.GenCodecMapping
import com.avsystem.commons.serialization.GenCodec

trait MongoFormatMapping[K[_]] extends GenCodecMapping[K] {
  def valueFormat[T](key: K[T]): MongoFormat[T]
  override def valueCodec[T](key: K[T]): GenCodec[T] = valueFormat(key).codec
}

trait MongoTypedKey[T] extends TypedKey[T] {
  def valueFormat: MongoFormat[T]
  override def valueCodec: GenCodec[T] = valueFormat.codec
}
object MongoTypedKey {
  given mongoFormatMapping[K[X] <: MongoTypedKey[X]]: MongoFormatMapping[K] =
    new MongoFormatMapping[K] {
      override def valueFormat[T](key: K[T]): MongoFormat[T] = key.valueFormat
    }
}
