package com.avsystem.commons
package kafka

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream}

import com.avsystem.commons.serialization.{GenCodec, StreamInput, StreamOutput}
import org.apache.kafka.common.serialization.{Deserializer, Serializer}

class KafkaSerde[T](codec: GenCodec[T]) extends AbstractSerde[T] {

  override def deserializer(): Deserializer[T] = new AbstractDeserializer[T] {
    override def deserialize(topic: String, data: Array[Byte]): T = {
      val in = new ByteArrayInputStream(data)
      val input = new StreamInput(new DataInputStream(in))
      codec.read(input)
    }
  }

  override def serializer(): Serializer[T] = new AbstractSerializer[T] {
    override def serialize(topic: String, data: T): Array[Byte] = {
      val out = new ByteArrayOutputStream()
      val output = new StreamOutput(new DataOutputStream(out))
      codec.write(output, data)
      out.toByteArray
    }
  }
}
