package com.avsystem.commons
package mongo.typed

import com.avsystem.commons.mongo.mongoId
import com.avsystem.commons.serialization.{StringWrapperCompanion, flatten, name}
import com.mongodb.reactivestreams.client.MongoClients
import monix.reactive.Observable
import org.bson.types.ObjectId

case class Wraper(str: String) extends AnyVal
object Wraper extends StringWrapperCompanion[Wraper]

case class RecordEntity(
  id: ObjectId,
  @name("stringy") str: String,
  ints: Seq[Int],
  mapencja: Map[Wraper, RecordEntity],
  maybeBool: Opt[Boolean],
  maybeSelf: Opt[RecordEntity]
) extends MongoEntity[ObjectId]
object RecordEntity extends MongoEntityCompanion[RecordEntity] {
  final val StrRef = SelfRef.ref(_.str)
  final val BoolRef = ref(_.maybeBool.get)
  final val DeepStrRef = ref(_.mapencja(Wraper("level1")).mapencja(Wraper("level2")).str)
}

@flatten sealed trait UnionEntity extends MongoEntity[ObjectId] {
  @mongoId def id: ObjectId
}
sealed trait MoreSpecificUnion extends UnionEntity {
  def record: RecordEntity
}
case class CaseOne(id: ObjectId, other: Int) extends UnionEntity
case class CaseTwo(id: ObjectId, other: Boolean, record: RecordEntity) extends MoreSpecificUnion
case class CaseThree(id: ObjectId, other: String, record: RecordEntity) extends MoreSpecificUnion
object UnionEntity extends MongoEntityCompanion[UnionEntity] {
  final val AsCaseOne = as[CaseOne]
  final val AsMoreSpecific = as[MoreSpecificUnion]
  final val RecordStrRef = as[MoreSpecificUnion].ref(_.record.str)
  final val RecordStrRef2 = as[MoreSpecificUnion].ref(_.record).ref(_.str)
}

case class ContainsUnion(
  id: ObjectId,
  union: UnionEntity
) extends MongoEntity[ObjectId]
object ContainsUnion extends MongoEntityCompanion[ContainsUnion] {
  final val MoreSpecificUnionRef = ref(_.union).as[MoreSpecificUnion]
  final val UnionRecordInts = ref(_.union).as[MoreSpecificUnion].ref(_.record.ints)

  final val IntsRef = MoreSpecificUnionRef.ref(_.record.ints)
  final val IntsHeadRef = MoreSpecificUnionRef.ref(_.record.ints.head)
  final val Filter = IntsRef.elemMatch(_.satisfiesOperators(c => Seq(c.gt(0), c.lt(10))))
  final val Filter2 = IntsRef(2).is(5)

  final val Update = IntsRef.push(sort = MongoOrder.ascending[Int]) & IntsRef.setOnInsert(Seq.empty)
}

object Testujo {
  def main(args: Array[String]): Unit = {
    import ContainsUnion._

    val client = MongoClients.create()
    val rawCollection = client.getDatabase("test").getCollection("containsUnion")
    val collection = new TypedMongoCollection[ContainsUnion](rawCollection)

    case class Partial(ints: Seq[Int], id: ObjectId)

    val partials: Observable[Partial] = collection.find(
      ref(_.union.id).is(new ObjectId),
      MongoProjection.zip(UnionRecordInts, IdRef).map(Partial.tupled),
      IdRef.ascending
    )

    println(ref(_.union).is[MoreSpecificUnion].toBson)
    println(Filter.toBson)
    println(Filter2.toBson)
    println((Filter && Filter2).toBson)
    println(Update.toBson)
    println(RecordEntity.BoolRef.is(true).toBson)
    println(RecordEntity.DeepStrRef.is("fu").toBson)
    println((UnionEntity.as[CaseOne].ref(_.other).is(0) || UnionEntity.as[CaseTwo].ref(_.other).is(true)).toBson)
  }
}
