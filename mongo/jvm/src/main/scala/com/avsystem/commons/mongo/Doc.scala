package com.avsystem.commons
package mongo

import org.bson.{BsonDocument, BsonValue}

/** @author
  *   MKej
  */
class Doc(private val doc: BsonDocument) extends AnyVal {
  def get[A, BSON <: BsonValue](key: DocKey[A, BSON]): Option[A] =
    Option(doc.get(key.key).asInstanceOf[BSON]).map(key.codec.fromBson)

  def getOpt[A, BSON <: BsonValue](key: DocKey[A, BSON]): Opt[A] =
    Opt(doc.get(key.key).asInstanceOf[BSON]).map(key.codec.fromBson)

  def require[A](key: DocKey[A, ? <: BsonValue]): A = getOpt(key).get

  def put[A](key: DocKey[A, ? <: BsonValue], value: A): Doc = {
    doc.put(key.key, key.codec.toBson(value))
    this
  }

  def putOpt[A](key: DocKey[A, ? <: BsonValue], optValue: Option[A]): Doc = optValue.fold(this)(put(key, _))

  def putOpt[A](key: DocKey[A, ? <: BsonValue], optValue: Opt[A]): Doc = optValue.fold(this)(put(key, _))

  def toBson: BsonDocument = doc
}

object Doc {
  def apply(): Doc = new Doc(new BsonDocument())
  def apply[A](key: DocKey[A, ? <: BsonValue], value: A): Doc = apply().put(key, value)
}
