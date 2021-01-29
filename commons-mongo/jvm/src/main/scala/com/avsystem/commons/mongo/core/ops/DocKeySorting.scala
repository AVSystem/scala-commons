package com.avsystem.commons
package mongo.core.ops

import com.avsystem.commons.mongo.DocKey
import org.bson.BsonValue

class DocKeySorting[T](val docKey: DocKey[T, _ <: BsonValue])
  extends AnyVal
    with BaseSorting
    with DocKeyKeyValueHandling[T]
