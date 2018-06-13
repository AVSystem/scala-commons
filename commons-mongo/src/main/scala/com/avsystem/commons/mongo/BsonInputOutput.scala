package com.avsystem.commons
package mongo

import java.nio.ByteBuffer

import com.avsystem.commons.serialization.{Input, Output}
import org.bson.types.ObjectId

trait BsonInput extends Any with Input {
  def readObjectId(): ObjectId
}

object BsonInput {
  def bigDecimalFromBytes(bytes: Array[Byte]): JBigDecimal = {
    val buf = ByteBuffer.wrap(bytes)
    val unscaledBytes = new Array[Byte](bytes.length - 4)
    buf.get(unscaledBytes)
    val unscaled = new JBigInteger(unscaledBytes)
    val scale = buf.getInt
    new JBigDecimal(unscaled, scale)
  }
}

trait BsonOutput extends Any with Output {
  def writeObjectId(objectId: ObjectId): Unit
}

object BsonOutput {
  def bigDecimalBytes(bigDecimal: JBigDecimal): Array[Byte] = {
    val unscaledBytes = bigDecimal.unscaledValue.toByteArray
    ByteBuffer.allocate(unscaledBytes.length + 4).put(unscaledBytes).putInt(bigDecimal.scale).array
  }
}
