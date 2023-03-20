package com.avsystem.commons
package mongo

import org.bson.BsonDocument

/**
  * @author MKej
  */
trait DocumentCodec[T] {
  def toDocument(t: T): Doc
  def fromDocument(doc: Doc): T

  lazy val bsonCodec: BsonCodec[T, BsonDocument] = BsonCodec.doc.map(fromDocument, toDocument)
}
