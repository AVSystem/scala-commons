package com.avsystem.commons
package kafka

import java.io.ByteArrayInputStream

import com.avsystem.commons.kafka.exceptions.UnsupportedVersionEvent
import com.avsystem.commons.serialization.GenCodec

class SerdeRegistry[T](codecs: Map[Byte, GenCodec[_ <: T]]) extends AbstractDeserializer[T] {

  private val registry = codecs.mapValues(new KafkaSerde(_))

  override def deserialize(topic: String, data: Array[Byte]): T = {
    val input = new ByteArrayInputStream(data)
    val version = input.read()
    if (version != -1) {
      registry.get(version.toByte) match {
        case Some(serde: KafkaSerde[T]) => serde.deserialize(input)
        case _ => throw new UnsupportedVersionEvent(s"Unsupported version ${data.head}, add proper serde to registry.")
      }
    } else {
      throw new UnsupportedVersionEvent(s"Error during deserialization, end of the stream reached.")
    }
  }
}
