package com.avsystem.commons
package mongo.core.ops

import com.mongodb.client.model.{PushOptions, Updates}
import org.bson.BsonValue
import org.bson.conversions.Bson

trait BaseIterableUpdating[E, C[T] <: Iterable[T]] extends Any with BaseUpdating[C[E]] {
  protected def encodeElement(e: E): BsonValue

  protected def useE(e: E)(f: (String, BsonValue) => Bson): Bson = {
    f(key, encodeElement(e))
  }

  protected def useEs(es: Seq[E])(f: (String, JList[BsonValue]) => Bson): Bson = {
    f(key, es.iterator.map(encodeElement).to[JList])
  }

  def addToSet(e: E): Bson = useE(e)(Updates.addToSet)
  def addEachToSet(es: E*): Bson = useEs(es)(Updates.addEachToSet)

  def push(e: E): Bson = useE(e)(Updates.push)
  //noinspection ConvertibleToMethodValue
  def pushEach(es: E*): Bson = useEs(es)(Updates.pushEach(_, _))
  def pushEachOptions(pushOptions: PushOptions, es: E*): Bson = useEs(es)(Updates.pushEach(_, _, pushOptions))

  def pull(e: E): Bson = useE(e)(Updates.pull)
  def pullAll(es: E*): Bson = useEs(es)(Updates.pullAll)

  def popFirst(): Bson = Updates.popFirst(key)
  def popLast(): Bson = Updates.popLast(key)
}
