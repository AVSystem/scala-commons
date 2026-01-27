package com.avsystem.commons
package mongo

import com.avsystem.commons.mongo.core.ops.{DocKeyFiltering, DocKeySorting}
import org.bson.{BsonDocument, BsonValue}

/** @author
  *   MKej
  */
case class DocKey[A, BSON <: BsonValue](key: String, codec: BsonCodec[A, BSON]) {
  def ++[B, BBSON <: BsonValue](other: DocKey[B, BBSON])(implicit ev: BSON <:< BsonDocument): DocKey[B, BBSON] =
    new DocKey(key + "." + other.key, other.codec)
}
