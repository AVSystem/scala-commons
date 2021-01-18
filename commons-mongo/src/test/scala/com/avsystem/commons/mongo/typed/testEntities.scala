package com.avsystem.commons
package mongo.typed

import com.avsystem.commons.misc.Timestamp
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
object InnerRecord extends MongoDataCompanion[InnerRecord] {
  final val Example = InnerRecord(
    24, "istr", Opt("istropt"), Opt.Empty, List(3, 4, 5), Map("ione" -> 1, "ithree" -> 3))
}

case class RecordTestEntity(
  id: String,
  int: Int,
  @name("str") renamedStr: String,
  tstamp: Timestamp,
  strOpt: Opt[String],
  @optionalParam intOpt: Opt[Int],
  intList: List[Int],
  intMap: Map[String, Int],
  inner: InnerRecord,
  innerOpt: Opt[InnerRecord],
  innerList: List[InnerRecord],
  innerMap: Map[InnerId, InnerRecord],
  complex: Opt[Map[InnerId, List[InnerRecord]]],
  @transientDefault union: UnionTestEntity = CaseOne("uid", "ustr", data = false)
) extends MongoEntity[String]
object RecordTestEntity extends MongoEntityCompanion[RecordTestEntity] {
  final val Example = RecordTestEntity(
    "rid", 42, "str", Timestamp.Zero, Opt("stropt"), Opt.Empty,
    List(1, 2, 3), Map("one" -> 1, "two" -> 2), InnerRecord.Example,
    Opt(InnerRecord.Example), List(InnerRecord.Example), Map(InnerId("iid") -> InnerRecord.Example),
    Opt(Map(InnerId("iid") -> List(InnerRecord.Example)))
  )
}

@flatten sealed trait UnionTestEntity extends MongoEntity[String] {
  def str: String
}
sealed trait HasInner extends UnionTestEntity {
  def inner: RecordTestEntity
}
case class CaseOne(id: String, str: String, data: Boolean) extends UnionTestEntity
case class CaseTwo(id: String, str: String, data: Int, inner: RecordTestEntity) extends HasInner
case class CaseThree(id: String, str: String, data: String, inner: RecordTestEntity) extends HasInner
object UnionTestEntity extends MongoEntityCompanion[UnionTestEntity]