package com.avsystem.commons
package mongo.typed

import com.avsystem.commons.misc.{AbstractValueEnum, AbstractValueEnumCompanion, EnumCtx}
import org.bson.{BsonDocument, BsonValue}

sealed trait MongoUpdateOperator[T] extends Product {

  import MongoUpdateOperator._

  def rawOperator: String = "$" + productPrefix.uncapitalize

  def toBson: BsonValue = this match {
    case Set(value, format) => format.writeBson(value)
    case Inc(value, format) => format.writeBson(value)
    case Min(value, format) => format.writeBson(value)
    case Max(value, format) => format.writeBson(value)
    case Mul(value, format) => format.writeBson(value)
    case CurrentDate(tpe) =>
      tpe.mapOr(Bson.boolean(true), dt => Bson.document("$type", Bson.string(dt.name.uncapitalize)))
    case Rename(newPath) => Bson.string(newPath)
    case SetOnInsert(value, format) => format.writeBson(value)
    case Unset() => Bson.string("")

    case push: Push[_, ct] =>
      val doc = new BsonDocument
      doc.put("$each", Bson.array(push.values.iterator.map(push.format.writeBson)))
      push.position.foreach(v => doc.put("$position", Bson.int(v)))
      push.slice.foreach(v => doc.put("$slice", Bson.int(v)))
      push.sort.foreach(v => doc.put("$sort", v.toBson))
      doc

    case addToSet: AddToSet[_, ct] =>
      Bson.document("$each", Bson.array(addToSet.values.iterator.map(addToSet.format.writeBson)))

    case pop: Pop[_, _] =>
      Bson.int(if (pop.first) -1 else 1)

    case pull: Pull[_, _] =>
      pull.filter.toBson

    case pullAll: PullAll[_, ct] =>
      Bson.array(pullAll.values.iterator.map(pullAll.format.writeBson))
  }
}
object MongoUpdateOperator {
  final class CurrentDateType(implicit enumCtx: EnumCtx) extends AbstractValueEnum
  object CurrentDateType extends AbstractValueEnumCompanion[CurrentDateType] {
    final val Timestamp, Date: Value = new CurrentDateType
  }

  final case class Set[T](value: T, format: MongoFormat[T]) extends MongoUpdateOperator[T]
  final case class Inc[T](value: T, format: MongoFormat[T]) extends MongoUpdateOperator[T]
  final case class Min[T](value: T, format: MongoFormat[T]) extends MongoUpdateOperator[T]
  final case class Max[T](value: T, format: MongoFormat[T]) extends MongoUpdateOperator[T]
  final case class Mul[T](value: T, format: MongoFormat[T]) extends MongoUpdateOperator[T]
  final case class CurrentDate[T](tpe: Opt[CurrentDateType]) extends MongoUpdateOperator[T]
  final case class Rename[T](newPath: String) extends MongoUpdateOperator[T]
  final case class SetOnInsert[T](value: T, format: MongoFormat[T]) extends MongoUpdateOperator[T]
  final case class Unset[T]() extends MongoUpdateOperator[T]

  final case class Push[C[X] <: Iterable[X], T](
    values: Iterable[T],
    position: Opt[Int],
    slice: Opt[Int],
    sort: Opt[MongoOrder[T]],
    format: MongoFormat[T]
  ) extends MongoUpdateOperator[C[T]]

  final case class AddToSet[C[X] <: Iterable[X], T](
    values: Iterable[T],
    format: MongoFormat[T]
  ) extends MongoUpdateOperator[C[T]]

  final case class Pop[C[X] <: Iterable[X], T](first: Boolean) extends MongoUpdateOperator[C[T]]

  final case class Pull[C[X] <: Iterable[X], T](filter: MongoFilter[T]) extends MongoUpdateOperator[C[T]]

  final case class PullAll[C[X] <: Iterable[X], T](
    values: Iterable[T], format: MongoFormat[T]
  ) extends MongoUpdateOperator[C[T]]
}
