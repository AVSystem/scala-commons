package com.avsystem.commons
package mongo.core.ops

import com.avsystem.commons.mongo.BsonRef
import com.avsystem.commons.serialization.GenCodec

class BsonRefIterableUpdating[E, C[T] <: Iterable[T]](val bsonRef: BsonRef[_, C[E]])(implicit val elementCodec: GenCodec[E])
  extends BaseIterableUpdating[E, C]
    with BsonRefKeyValueHandling[C[E]]
    with BsonRefKeyElementHandling[E, C]
