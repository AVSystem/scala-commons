package com.avsystem.commons
package mongo.core

import com.avsystem.commons.mongo.GenCodecBasedBsonCodec
import com.avsystem.commons.serialization.GenCodec
import org.bson.codecs.configuration.{CodecRegistries, CodecRegistry}

object GenCodecRegistry {
  def create[T: ClassTag : GenCodec](baseRegistry: CodecRegistry): CodecRegistry = {
    val genRegistry = CodecRegistries.fromCodecs(new GenCodecBasedBsonCodec[T]())
    CodecRegistries.fromRegistries(genRegistry, baseRegistry)
  }
}
