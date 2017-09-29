package com.avsystem.commons
package mongo.core

import com.avsystem.commons.serialization.GenCodec
import org.bson.codecs.configuration.{CodecRegistries, CodecRegistry}

object GenCodecRegistry {
  def create[T: ClassTag : GenCodec](baseRegistry: CodecRegistry): CodecRegistry = {
    val genProvider = new GenCodecProvider[T]()
    val genRegistry = CodecRegistries.fromProviders(genProvider)
    CodecRegistries.fromRegistries(genRegistry, baseRegistry)
  }
}
