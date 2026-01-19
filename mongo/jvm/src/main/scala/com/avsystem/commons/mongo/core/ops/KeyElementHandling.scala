package com.avsystem.commons
package mongo.core.ops

import org.bson.BsonValue
import org.bson.conversions.Bson

trait KeyElementHandling[E] extends Any with KeyHandling {
  protected def encodeElement(e: E): BsonValue

  protected def useE(e: E)(f: (String, BsonValue) => Bson): Bson =
    f(key, encodeElement(e))

  protected def useEs(es: Seq[E])(f: (String, JList[BsonValue]) => Bson): Bson =
    f(key, es.iterator.map(encodeElement).to(JList))
}
