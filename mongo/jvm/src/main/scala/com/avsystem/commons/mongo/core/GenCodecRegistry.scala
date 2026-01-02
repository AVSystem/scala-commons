package com.avsystem.commons
package mongo.core

import com.avsystem.commons.serialization.GenCodec
import org.bson.codecs.configuration.{CodecRegistries, CodecRegistry}

object GenCodecRegistry {
  final val LegacyOptionEncoding: Boolean =
    System.getProperty("commons.mongo.legacyOptionEncoding").opt.fold(false)(_.toBoolean)

  def create[T: ClassTag: GenCodec](
    baseRegistry: CodecRegistry,
    legacyOptionEncoding: Boolean = LegacyOptionEncoding,
  ): CodecRegistry = {
    val genProvider = new GenCodecProvider[T](legacyOptionEncoding)
    val genRegistry = CodecRegistries.fromProviders(genProvider)
    CodecRegistries.fromRegistries(genRegistry, baseRegistry)
  }
}
