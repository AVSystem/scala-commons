package com.avsystem.commons
package mongo.core.ops

import com.avsystem.commons.mongo.BsonValueOutput
import com.avsystem.commons.serialization.GenCodec
import org.bson.BsonValue

trait BsonRefKeyElementHandling[E, C[T] <: Iterable[T]] extends KeyElementHandling[E] with BsonRefKeyHandling[C[E]] {
  protected given GenCodec[E] = compiletime.defered

  override protected def encodeElement(e: E): BsonValue = BsonValueOutput.write(e)
}
