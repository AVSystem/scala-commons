package com.avsystem.commons
package kafka

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream}

import com.avsystem.commons.serialization.{GenCodec, StreamInput, StreamOutput}

class KafkaSerde[T](codec: GenCodec[T]) {

  def deserialize(topic: String, data: Array[Byte]): T = {
    val in = new ByteArrayInputStream(data)
    val input = new StreamInput(new DataInputStream(in))
    codec.read(input)
  }

  def serialize(prefix: Byte, topic: String, data: T): Array[Byte] = {
    val out = new ByteArrayOutputStream()
    out.write(prefix)
    val output = new StreamOutput(new DataOutputStream(out))
    codec.write(output, data)
    out.toByteArray
  }
}
