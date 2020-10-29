package com.avsystem.commons
package mongo.typed

import com.avsystem.commons.mongo.mongoId
import com.avsystem.commons.serialization.{flatten, name}
import com.mongodb.reactivestreams.client.MongoClients
import org.bson.types.ObjectId

case class RecordEntity(
  id: ObjectId,
  @name("stringy") str: String,
  ints: Seq[Int],
  maybeBool: Opt[Boolean],
  maybeSelf: Opt[RecordEntity]
) extends MongoEntity[ObjectId]
object RecordEntity extends MongoEntityCompanion[RecordEntity] {
  final val StrRef = SelfRef.ref(_.str)
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

  final val IntsRef = MoreSpecificUnionRef.ref(_.record.ints)
  final val Filter = IntsRef.elemMatch(_.satisfiesOperators(c => Seq(c.gt(0), c.lt(10))))
  final val Filter2 = IntsRef.containsAny(1, 2, 3)

  final val Update = IntsRef.push(sort = MongoOrder.ascending[Int]) & IntsRef.setOnInsert(Seq.empty)
}

object Testujo {
  def main(args: Array[String]): Unit = {
    val client = MongoClients.create()
    val rawCollection = client.getDatabase("test").getCollection("containsUnion")
    val collection = new TypedMongoCollection[ContainsUnion](rawCollection)

    val tuples = collection.find(
      ContainsUnion.ref(_.union.id).is(new ObjectId),
      ContainsUnion.tupleProjection(ContainsUnion.ref(_.union).as[MoreSpecificUnion].ref(_.record.ints), ContainsUnion.IdRef),
      ContainsUnion.IdRef.ascendingSortOrder
    )

    println(ContainsUnion.ref(_.union).is[MoreSpecificUnion].toBson)
    println(ContainsUnion.Filter.toBson)
    println(ContainsUnion.Filter2.toBson)
    println((ContainsUnion.Filter && ContainsUnion.Filter2).toBson)
    println(ContainsUnion.Update.toBson)
  }
}
