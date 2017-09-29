package com.avsystem.commons
package mongo.core.ops

import com.avsystem.commons.mongo.{BsonRef, DocKey}
import org.bson.BsonValue

trait KeyGetter[T] {
  def keyOf(t: T): String
}

object KeyGetter {
  private object bsonRefKeyGetter extends KeyGetter[BsonRef[_]] {
    override def keyOf(t: BsonRef[_]): String = t.path
  }

  private object docKeyKeyGetter extends KeyGetter[DocKey[_, _]] {
    override def keyOf(t: DocKey[_, _]): String = t.key
  }

  implicit def docKey[A, BSON <: BsonValue]: KeyGetter[DocKey[A, BSON]] = {
    docKeyKeyGetter.asInstanceOf[KeyGetter[DocKey[A, BSON]]]
  }

  implicit def bsonRef[T]: KeyGetter[BsonRef[T]] = bsonRefKeyGetter.asInstanceOf[KeyGetter[BsonRef[T]]]
}
