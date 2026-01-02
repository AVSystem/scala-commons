package com.avsystem.commons
package mongo.core.ops

import org.bson.conversions.Bson
import org.mongodb.scala.MongoClient
import org.mongodb.scala.bson.BsonDocument
import org.scalactic.Equality

object BsonEquality extends Equality[Bson] {
  def toDoc(bson: Bson): BsonDocument = bson.toBsonDocument(classOf[BsonDocument], MongoClient.DEFAULT_CODEC_REGISTRY)

  override def areEqual(a: Bson, b: Any): Boolean =
    (a, b) match {
      case (null, null) =>
        true
      case (aBson, bBson: Bson) =>
        toDoc(aBson) == toDoc(bBson)
      case _ =>
        false
    }

  implicit def bsonEquality: BsonEquality.type = this
}
