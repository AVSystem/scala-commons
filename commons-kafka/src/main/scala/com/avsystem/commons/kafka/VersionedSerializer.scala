package com.avsystem.commons
package kafka

import java.io.ByteArrayOutputStream

import com.avsystem.commons.serialization.GenCodec

class VersionedSerializer[T](version: Byte)(implicit codec: GenCodec[T]) extends AbstractSerializer[T] {

  private val serde = new KafkaSerde(codec)

  override def serialize(topic: String, data: T): Array[Byte] = {
    val output = new ByteArrayOutputStream()
    output.write(version)
    serde.serialize(output, data)
  }
}
