package com.avsystem.commons
package kafka

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream}

import com.avsystem.commons.serialization.{GenCodec, StreamInput, StreamOutput}

private[kafka] class CodecSerde[T](codec: GenCodec[T]) {

  def deserialize(in: ByteArrayInputStream): T = {
    val input = new StreamInput(new DataInputStream(in))
    codec.read(input)
  }

  def serialize(out: ByteArrayOutputStream, data: T): Array[Byte] = {
    val output = new StreamOutput(new DataOutputStream(out))
    codec.write(output, data)
    out.toByteArray
  }
}
