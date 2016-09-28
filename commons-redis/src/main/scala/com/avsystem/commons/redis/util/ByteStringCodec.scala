package com.avsystem.commons
package redis.util

import java.io.{DataInputStream, DataOutputStream}

import akka.util._
import com.avsystem.commons.serialization.{GenCodec, StreamInput, StreamOutput}

/**
  * Author: ghik
  * Created: 27/09/16.
  */
object ByteStringCodec {
  def write[T: GenCodec](value: T): ByteString = {
    val builder = new ByteStringBuilder
    GenCodec.write(new StreamOutput(new DataOutputStream(builder.asOutputStream)), value)
    builder.result()
  }

  def read[T: GenCodec](bytes: ByteString): T =
    GenCodec.read(new StreamInput(new DataInputStream(bytes.iterator.asInputStream)))
}
