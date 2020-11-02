package com.avsystem.commons
package mongo.typed

import com.avsystem.commons.serialization._

case class RecordId(id: String) extends AnyVal
object RecordId extends StringWrapperCompanion[RecordId]

case class InnerId(id: String) extends AnyVal
object InnerId extends StringWrapperCompanion[InnerId]

case class InnerRecord(
  int: Int,
  @name("str") renamedStr: String,
  strOpt: Opt[String],
  @optionalParam intOpt: Opt[Int],
  intList: List[Int],
  intMap: Map[String, Int]
)
object InnerRecord extends MongoDataCompanion[InnerRecord]

case class RecordTestEntity(
  id: String,
  int: Int,
  @name("str") renamedStr: String,
  strOpt: Opt[String],
  @optionalParam intOpt: Opt[Int],
  intList: List[Int],
  intMap: Map[String, Int],
  inner: InnerRecord,
  innerOpt: Opt[InnerRecord],
  innerList: List[InnerRecord],
  innerMap: Map[InnerId, InnerRecord],
  complex: Opt[Map[InnerId, List[InnerRecord]]],
  union: UnionTestEntity
) extends MongoEntity[String]
object RecordTestEntity extends MongoEntityCompanion[RecordTestEntity]

@flatten sealed trait UnionTestEntity extends MongoEntity[String] {
  def str: String
}
object UnionTestEntity extends MongoEntityCompanion[UnionTestEntity] {
  sealed trait HasInner extends UnionTestEntity {
    def inner: RecordTestEntity
  }

  case class CaseOne(id: String, str: String, data: Boolean) extends UnionTestEntity
  case class CaseTwo(id: String, str: String, data: Int, inner: RecordTestEntity) extends HasInner
  case class CaseThree(id: String, str: String, data: String, inner: RecordTestEntity) extends HasInner
}
