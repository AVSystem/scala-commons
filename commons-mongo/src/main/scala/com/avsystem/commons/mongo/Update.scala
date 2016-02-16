package com.avsystem.commons
package mongo

import com.avsystem.commons.jiop.JavaInterop._
import com.mongodb.client.model.{Updates => U}
import org.bson.conversions.Bson

/**
  * @author MKej
  */
object Update {
  def combine(updates: Bson*): Bson = U.combine(updates.asJava)

  def set[A](key: DocKey[A, _], value: A): Bson = U.set(key.key, key.codec.toBson(value))
  def unset(key: DocKey[_, _]): Bson = U.unset(key.key)

  def max[A](key: DocKey[A, _], value: A): Bson = U.max(key.key, key.codec.toBson(value))
}
