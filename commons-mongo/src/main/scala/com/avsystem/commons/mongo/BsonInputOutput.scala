package com.avsystem.commons
package mongo

import java.nio.ByteBuffer

import com.avsystem.commons.serialization.{Input, Output}
import org.bson.types.ObjectId

trait BsonInput extends Any with Input {
  def readObjectId(): ObjectId
}

object BsonInput {
  def bigDecimalFromBytes(bytes: Array[Byte]): BigDecimal = {
    val buf = ByteBuffer.wrap(bytes)
    val unscaledBytes = new Array[Byte](bytes.length - 4)
    buf.get(unscaledBytes)
    val unscaled = BigInt(unscaledBytes)
    val scale = buf.getInt
    BigDecimal(unscaled, scale)
  }
}

trait BsonOutput extends Any with Output {
  def writeObjectId(objectId: ObjectId): Unit
}

object BsonOutput {
  def bigDecimalBytes(bigDecimal: BigDecimal): Array[Byte] = {
    val unscaledBytes = bigDecimal.bigDecimal.unscaledValue.toByteArray
    ByteBuffer.allocate(unscaledBytes.length + 4).put(unscaledBytes).putInt(bigDecimal.scale).array
  }
}
