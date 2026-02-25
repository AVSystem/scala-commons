package com.avsystem.commons
package mongo.core.ops

import org.bson.conversions.Bson
import org.scalactic.Equality

object BsonEquality extends Equality[Bson] {

  override def areEqual(a: Bson, b: Any): Boolean =
    (a, b) match {
      case (null, null) =>
        true
      case (aBson, bBson: Bson) =>
        aBson.toBsonDocument == bBson.toBsonDocument
      case _ =>
        false
    }

  implicit def bsonEquality: BsonEquality.type = this
}
