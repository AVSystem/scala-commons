package com.avsystem.commons
package mongo

import com.mongodb.client.model.{Sorts => S}
import org.bson.conversions.Bson

/** @author
  *   MKej
  */
object Sort {
  def ascending(keys: DocKey[?, ?]*): Bson = S.ascending(keys.map(_.key).asJava)
  def descending(keys: DocKey[?, ?]*): Bson = S.descending(keys.map(_.key).asJava)
}
