package com.avsystem.commons
package mongo.model

import com.avsystem.commons.mongo.mongoId
import com.avsystem.commons.serialization.{flatten, name}
import org.bson.types.ObjectId

case class RecordEntity(
  @mongoId id: ObjectId,
  @name("stringy") str: String,
  ints: Seq[Int],
  maybeBool: Opt[Boolean],
  maybeSelf: Opt[RecordEntity]
)
object RecordEntity extends MongoDataCompanion[RecordEntity] {
  final val IdRef = ref(_.id)
  final val StrRef = SelfRef.ref(_.str)
}

@flatten sealed trait UnionEntity {
  @mongoId def id: ObjectId
}
sealed trait MoreSpecificUnion extends UnionEntity {
  def record: RecordEntity
}
case class CaseOne(id: ObjectId, other: Int) extends UnionEntity
case class CaseTwo(id: ObjectId, other: Boolean, record: RecordEntity) extends MoreSpecificUnion
case class CaseThree(id: ObjectId, other: String, record: RecordEntity) extends MoreSpecificUnion
object UnionEntity extends MongoDataCompanion[UnionEntity] {
  final val IdRef = ref(_.id)

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
}

object Testujo {
  def main(args: Array[String]): Unit = {
    println(ContainsUnion.ref(_.union).is[MoreSpecificUnion].toBson)
    println(ContainsUnion.Filter.toBson)
    println(ContainsUnion.Filter2.toBson)
    println((ContainsUnion.Filter && ContainsUnion.Filter2).toBson)
  }
}
