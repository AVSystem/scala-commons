package com.avsystem.commons
package mongo.core.ops

import com.avsystem.commons.mongo.{BsonRef, DocKey}

trait KeyGetter[-T] {
  def keyOf(t: T): String
}

object KeyGetter {
  given bsonRefKeyGetter: KeyGetter[BsonRef[_, _]] with {
    override def keyOf(t: BsonRef[_, _]): String = t.path
  }

  given docKeyKeyGetter: KeyGetter[DocKey[_, _]] with {
    override def keyOf(t: DocKey[_, _]): String = t.key
  }
}
