package com.avsystem.commons
package mongo.typed

import com.avsystem.commons.misc.{AutoNamedEnum, NamedEnumCompanion, Timestamp, TypedMap}
import com.avsystem.commons.serialization._
import org.bson.types.ObjectId

case class RecordId(id: String) extends AnyVal
object RecordId extends StringWrapperCompanion[RecordId]

case class InnerId(id: String) extends AnyVal
object InnerId extends StringWrapperCompanion[InnerId]

sealed abstract class PKey[T](implicit val valueFormat: MongoFormat[T]) extends MongoTypedKey[T] with AutoNamedEnum
object PKey extends NamedEnumCompanion[PKey[_]] {
  case object IntKey extends PKey[Int]
  case object StringKey extends PKey[String]
  case object InnerKey extends PKey[InnerRecord]

  val values: List[PKey[_]] = caseObjects
}

case class InnerRecord(
  int: Int,
  @name("str") renamedStr: String,
  strOpt: Opt[String],
  @optionalParam intOpt: Opt[Int],
  intList: List[Int],
  intMap: Map[String, Int],
)
object InnerRecord extends MongoDataCompanion[InnerRecord] {
  final val Example = InnerRecord(24, "istr", Opt("istropt"), Opt.Empty, List(3, 4, 5), Map("ione" -> 1, "ithree" -> 3))
}

case class Props(map: Map[String, String]) extends AnyVal
object Props extends TransparentWrapperCompanion[Map[String, String], Props]

case class RecordTestEntity(
  id: String,
  int: Int,
  @name("str") renamedStr: String,
  tstamp: Timestamp,
  strOpt: Opt[String],
  @optionalParam intOpt: Opt[Int],
  intList: List[Int],
  intMap: Map[String, Int],
  typedMap: TypedMap[PKey],
  inner: InnerRecord,
  innerOpt: Opt[InnerRecord],
  innerList: List[InnerRecord],
  innerMap: Map[InnerId, InnerRecord],
  complex: Opt[Map[InnerId, List[InnerRecord]]],
  props: Props,
  @transientDefault union: UnionTestEntity = CaseOne("uid", "ustr", data = false),
) extends MongoEntity[String]
object RecordTestEntity extends MongoEntityCompanion[RecordTestEntity] {
  final val Example = RecordTestEntity(
    "rid",
    42,
    "str",
    Timestamp.Zero,
    Opt("stropt"),
    Opt.Empty,
    List(1, 2, 3),
    Map("one" -> 1, "two" -> 2),
    TypedMap(PKey.IntKey -> 42, PKey.InnerKey -> InnerRecord.Example),
    InnerRecord.Example,
    Opt(InnerRecord.Example),
    List(InnerRecord.Example),
    Map(InnerId("iid") -> InnerRecord.Example),
    Opt(Map(InnerId("iid") -> List(InnerRecord.Example))),
    Props(Map.empty),
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

case class TestAutoId(id: ObjectId) extends AnyVal
object TestAutoId extends ObjectIdWrapperCompanion[TestAutoId]

case class RecordTestAutoIdEntity(
  str: String,
  int: Int,
) extends AutoIdMongoEntity[TestAutoId]
object RecordTestAutoIdEntity extends MongoEntityCompanion[RecordTestAutoIdEntity]

case class AutoObjectIdEntity(
  str: String,
  int: Int,
) extends AutoIdMongoEntity[ObjectId]
object AutoObjectIdEntity extends MongoEntityCompanion[AutoObjectIdEntity]
