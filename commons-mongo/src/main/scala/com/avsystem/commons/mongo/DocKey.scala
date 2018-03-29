package com.avsystem.commons
package mongo

import com.avsystem.commons.mongo.core.ops.{DocKeyFiltering, DocKeySorting}
import org.bson.{BsonDocument, BsonValue}

/**
  * @author MKej
  */
case class DocKey[A, BSON <: BsonValue](key: String, codec: BsonCodec[A, BSON]) {
  def ++[B, BBSON <: BsonValue](other: DocKey[B, BBSON])(implicit ev: BSON <:< BsonDocument): DocKey[B, BBSON] =
    new DocKey(key + "." + other.key, other.codec)
}
object DocKey {
  implicit def docKeySorting[T](docKey: DocKey[T, _ <: BsonValue]): DocKeySorting[T] = new DocKeySorting(docKey)
  implicit def docKeyFiltering[T](docKey: DocKey[T, _ <: BsonValue]): DocKeyFiltering[T] = new DocKeyFiltering(docKey)
}
