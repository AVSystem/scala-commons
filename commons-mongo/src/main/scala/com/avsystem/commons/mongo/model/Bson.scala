package com.avsystem.commons
package mongo.model

import org.bson._

object Bson {
  final val And = "$and"
  final val Or = "$or"
  final val Nor = "$nor"
  final val Exists = "$exists"
  final val Type = "$type"
  final val Size = "$size"
  final val Not = "$not"
  final val Eq = "$eq"
  final val Ne = "$ne"
  final val In = "$in"
  final val Nin = "$nin"
  final val Gt = "$gt"
  final val Gte = "$gte"
  final val Lt = "$lt"
  final val Lte = "$lte"
  final val ElemMatch = "$elemMatch"
  final val All = "$all"

  def document(key: String, value: BsonValue): BsonDocument =
    new BsonDocument(key, value)

  def document(keyValues: (String, BsonValue)*): BsonDocument =
    document(keyValues)

  def document(keyValues: Iterable[(String, BsonValue)]): BsonDocument =
    document(keyValues.iterator)

  def document(keyValues: Iterator[(String, BsonValue)]): BsonDocument = {
    val doc = new BsonDocument
    keyValues.foreach { case (key, value) =>
      doc.append(key, value)
    }
    doc
  }

  def array(values: BsonValue*): BsonArray =
    array(values)

  def array(values: Iterable[BsonValue]): BsonArray =
    array(values.iterator)

  def array(values: Iterator[BsonValue]): BsonArray = {
    val array = new BsonArray
    values.foreach(array.add)
    array
  }

  def boolean(value: Boolean): BsonBoolean =
    new BsonBoolean(value)

  def int(value: Int): BsonInt32 =
    new BsonInt32(value)

  def long(value: Long): BsonInt64 =
    new BsonInt64(value)

  def string(value: String): BsonString =
    new BsonString(value)
}
