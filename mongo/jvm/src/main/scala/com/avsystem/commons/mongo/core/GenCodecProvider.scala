package com.avsystem.commons
package mongo.core

import com.avsystem.commons.mongo.GenCodecBasedBsonCodec
import com.avsystem.commons.serialization.GenCodec
import org.bson.codecs.Codec
import org.bson.codecs.configuration.{CodecProvider, CodecRegistry}

class GenCodecProvider[T: GenCodec](legacyOptionEncoding: Boolean)(implicit ct: ClassTag[T]) extends CodecProvider {
  private val mongoCodec = new GenCodecBasedBsonCodec[T](legacyOptionEncoding)
  private val runtimeClass = ct.runtimeClass

  override def get[X](clazz: Class[X], registry: CodecRegistry): Codec[X] =
    if (runtimeClass.isAssignableFrom(clazz)) mongoCodec.asInstanceOf[Codec[X]]
    else null
}
