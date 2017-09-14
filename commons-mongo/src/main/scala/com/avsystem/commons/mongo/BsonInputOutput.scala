package com.avsystem.commons
package mongo

import com.avsystem.commons.serialization.{Input, Output}
import org.bson.types.ObjectId

trait BsonInput extends Input {
  def readObjectId(): ObjectId
}

trait BsonOutput extends Output {
  def writeObjectId(objectId: ObjectId): Unit
}
