package com.avsystem.commons
package mongo.core.ops

import com.avsystem.commons.mongo.DocKey
import org.bson.BsonValue

trait DocKeyKeyHandling[T] extends Any with KeyHandling {
  protected def docKey: DocKey[T, ? <: BsonValue]

  override protected def key: String = docKey.key
}
