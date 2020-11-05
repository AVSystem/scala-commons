package com.avsystem.commons
package mongo.typed

import com.avsystem.commons.misc.{AbstractValueEnum, AbstractValueEnumCompanion, EnumCtx}
import org.bson.{BsonDocument, BsonValue}


case class MongoIndex[E](fields: Vector[(MongoPropertyRef[E, _], MongoIndexType)]) {
  require(fields.nonEmpty, "MongoDB index cannot be empty")

  def concat(other: MongoIndex[E]): MongoIndex[E] =
    MongoIndex(fields ++ other.fields)

  def ++(other: MongoIndex[E]): MongoIndex[E] =
    concat(other)

  def toBson: BsonDocument = {
    val doc = new BsonDocument
    fields.foreach { case (ref, tpe) =>
      val path = ref.filterPath
      if (doc.containsKey(path)) {
        throw new IllegalArgumentException(s"duplicate field $path in MongoDB index")
      }
      doc.put(path, tpe.toBson)
    }
    doc
  }
}
object MongoIndex {
  def apply[E](fields: (MongoPropertyRef[E, _], MongoIndexType)*): MongoIndex[E] =
    MongoIndex(fields.toVector)

  def ascending[E](fields: MongoPropertyRef[E, _]*): MongoIndex[E] =
    MongoIndex(fields.iterator.map(f => f -> MongoIndexType.Ascending).toVector)

  def descending[E](fields: MongoPropertyRef[E, _]*): MongoIndex[E] =
    MongoIndex(fields.iterator.map(f => f -> MongoIndexType.Ascending).toVector)
}

final class MongoIndexType(implicit enumCtx: EnumCtx) extends AbstractValueEnum {

  import MongoIndexType._

  def toBson: BsonValue = this match {
    case Ascending => Bson.int(1)
    case Descending => Bson.int(-1)
    case Hashed => Bson.string("hashed")
    case Text => Bson.string("text")
    case TwoDim => Bson.string("2d")
    case TwoDimSphere => Bson.string("2dsphere")
    case GeoHaystack => Bson.string("geoHaystack")
  }
}
object MongoIndexType extends AbstractValueEnumCompanion[MongoIndexType] {
  final val Ascending, Descending, Hashed, Text, TwoDim, TwoDimSphere, GeoHaystack: Value = new MongoIndexType
}
