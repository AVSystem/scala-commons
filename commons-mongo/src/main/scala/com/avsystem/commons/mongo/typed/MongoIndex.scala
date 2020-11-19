package com.avsystem.commons
package mongo.typed

import com.avsystem.commons.misc.{AbstractValueEnum, AbstractValueEnumCompanion, EnumCtx}
import com.mongodb.client.model.IndexOptions
import org.bson.{BsonDocument, BsonValue}

/**
  * Represents a MongoDB index, expressed as sequence of fields associated with index type.
  * Note: additional index options are passed separately into methods like [[TypedMongoCollection.createIndex]].
  *
  * Examples:
  *
  * {{{
  *   case class MyEntity(id: String, int: Int, num: Double) extends MongoEntity[String]
  *   object MyEntity extends MongoEntityCompanion[MyEntity]
  *
  *   // {"int": 1}
  *   MyEntity.ref(_.int).ascendingIndex
  *
  *   // {"int": -1}
  *   MyEntity.ref(_.int).descendingIndex
  *
  *   // {"int": 1, "num": 1}
  *   MongoIndex.ascending(MyEntity.ref(_.int), MyEntity.ref(_.num))
  *
  *   // {"int": 1, "num": -1, "_id": "hashed"}
  *   import MongoIndexType._
  *   MongoIndex(
  *     MyEntity.ref(_.int) -> Ascending,
  *     MyEntity.ref(_.num) -> Descending,
  *     MyEntity.ref(_.id) -> Hashed
  *   )
  * }}}
  *
  * @tparam E type of the MongoDB entity
  */
case class MongoIndex[E](
  fields: Vector[(MongoPropertyRef[E, _], MongoIndexType)],
  setupOptions: IndexOptions => IndexOptions = identity
) {
  require(fields.nonEmpty, "MongoDB index cannot be empty")

  def withOptions(moreSetupOptions: IndexOptions => IndexOptions): MongoIndex[E] =
    copy(setupOptions = setupOptions.andThen(moreSetupOptions))

  def toBson: BsonDocument = {
    val doc = new BsonDocument
    fields.foreach { case (ref, tpe) =>
      val path = ref.rawPath
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
    MongoIndex(fields.iterator.map(f => f -> MongoIndexType.Descending).toVector)
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
