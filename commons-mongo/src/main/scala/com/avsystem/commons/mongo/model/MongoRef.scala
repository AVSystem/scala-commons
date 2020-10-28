package com.avsystem.commons
package mongo.model

import com.avsystem.commons.annotation.macroPrivate
import com.avsystem.commons.macros.serialization.MongoMacros
import com.avsystem.commons.mongo.model.MongoRef.{FieldRef, PropertyAsSubtype}
import com.avsystem.commons.mongo.{BsonValueInput, KeyEscaper}
import org.bson.{BsonDocument, BsonType, BsonValue}

trait HasRefMacros[E, T] extends Any {
  type ThisDataRef[T0 <: T] <: MongoRef[E, T0]

  @macroPrivate def thisDataRef: ThisDataRef[T]

  // this macro effectively calls `fieldRefFor` while doing some additional static checks
  def ref[T0](fun: T => T0): MongoPropertyRef[E, T0] = macro MongoMacros.refImpl

  // this macro effectively calls `subtypeRefFor` while doing some additional static checks
  def as[C <: T]: ThisDataRef[C] = macro MongoMacros.asSubtype[C]

  // this macro effectively calls `subtypeConditionFor` while doing some additional static checks
  def is[C <: T]: MongoDocumentFilter[E] = macro MongoMacros.isSubtype[C]
}

sealed trait MongoProjection[E, T] {
  def impliedFilter: MongoDocumentFilter[E]

  def toProjectionBson: BsonDocument
  def decode(doc: BsonDocument): T
}

sealed trait MongoRef[E, T] extends MongoProjection[E, T] with HasRefMacros[E, T] { self =>
  def format: MongoFormat[T]

  @macroPrivate def subtypeRefFor[C <: T : ClassTag]: ThisDataRef[C]

  @macroPrivate def fieldRefFor[T0](scalaFieldName: String): MongoPropertyRef[E, T0] =
    format.assumeAdt.fieldRefFor(this, scalaFieldName)

  @macroPrivate def subtypeConditionFor[C <: T : ClassTag]: MongoDocumentFilter[E] =
    format.assumeUnion.subtypeConditionFor(this, classTag[C].runtimeClass.asInstanceOf[Class[C]])
}

sealed trait MongoDataRef[E, T <: E] extends MongoRef[E, T] {
  type ThisDataRef[C <: T] = MongoDataRef[E, C]

  @macroPrivate final def thisDataRef: ThisDataRef[T] = this

  def fullFormat: MongoAdtFormat[E]
  def format: MongoAdtFormat[T]

  @macroPrivate def subtypeRefFor[C <: T : ClassTag]: MongoDataRef[E, C] =
    format.assumeUnion.subtypeRefFor(this, classTag[C].runtimeClass.asInstanceOf[Class[C]])

  def toProjectionBson: BsonDocument = Bson.document()
  def decode(doc: BsonDocument): T = BsonValueInput.read(doc)(format.codec)
}

sealed trait MongoPropertyRef[E, T] extends MongoRef[E, T] {
  type ThisDataRef[T0 <: T] = MongoPropertyRef[E, T0]

  @macroPrivate final def thisDataRef: ThisDataRef[T] = this

  @macroPrivate def subtypeRefFor[C <: T : ClassTag]: MongoPropertyRef[E, C] =
    format.assumeUnion.subtypeRefFor(this, classTag[C].runtimeClass.asInstanceOf[Class[C]])

  private[mongo] def filterCreator: MongoFilter.Creator[T] =
    new MongoFilter.Creator(format)

  def satisfies(filter: MongoFilter.Creator[T] => MongoFilter[T]): MongoDocumentFilter[E] =
    satisfies(filter(new MongoFilter.Creator[T](format)))

  def satisfies(filter: MongoFilter[T]): MongoDocumentFilter[E] =
    MongoDocumentFilter.PropertyValueFilter(this, filter)

  def not(filter: MongoFilter.Creator[T] => MongoOperatorsFilter[T]): MongoDocumentFilter[E] =
    not(filter(new MongoFilter.Creator[T](format)))

  def not(filter: MongoOperatorsFilter[T]): MongoDocumentFilter[E] =
    satisfies(filter.negated)

  def exists: MongoDocumentFilter[E] =
    exists(true)

  def exists(exists: Boolean): MongoDocumentFilter[E] =
    satisfies(filterCreator.exists(exists))

  def hasType(bsonType: BsonType): MongoDocumentFilter[E] =
    satisfies(filterCreator.hasType(bsonType))

  def is(value: T): MongoDocumentFilter[E] =
    satisfies(filterCreator.is(value))

  def isNot(value: T): MongoDocumentFilter[E] =
    satisfies(filterCreator.isNot(value))

  def in(values: Iterable[T]): MongoDocumentFilter[E] =
    satisfies(filterCreator.in(values))

  def in(values: T*): MongoDocumentFilter[E] = in(values)

  def notIn(values: Iterable[T]): MongoDocumentFilter[E] =
    satisfies(filterCreator.nin(values))

  def notIn(values: T*): MongoDocumentFilter[E] = notIn(values)

  def gt(value: T): MongoDocumentFilter[E] =
    satisfies(filterCreator.gt(value))

  def >(value: T): MongoDocumentFilter[E] = gt(value)

  def gte(value: T): MongoDocumentFilter[E] =
    satisfies(filterCreator.gte(value))

  def >=(value: T): MongoDocumentFilter[E] = gte(value)

  def lt(value: T): MongoDocumentFilter[E] =
    satisfies(filterCreator.lt(value))

  def <(value: T): MongoDocumentFilter[E] = lt(value)

  def lte(value: T): MongoDocumentFilter[E] =
    satisfies(filterCreator.lte(value))

  def <=(value: T): MongoDocumentFilter[E] = lte(value)

  def rawFilter(bson: BsonDocument): MongoDocumentFilter[E] =
    satisfies(filterCreator.rawFilter(bson))

  def valueSatisfies(
    exists: OptArg[Boolean] = OptArg.Empty,
    bsonType: OptArg[BsonType] = OptArg.Empty,
    eq: OptArg[T] = OptArg.Empty,
    ne: OptArg[T] = OptArg.Empty,
    gt: OptArg[T] = OptArg.Empty,
    gte: OptArg[T] = OptArg.Empty,
    lt: OptArg[T] = OptArg.Empty,
    lte: OptArg[T] = OptArg.Empty,
    in: OptArg[Iterable[T]] = OptArg.Empty,
    nin: OptArg[Iterable[T]] = OptArg.Empty,
    not: OptArg[MongoOperatorsFilter[T]] = OptArg.Empty
  ): MongoDocumentFilter[E] =
    satisfies(filterCreator.satisfies(exists, bsonType, eq, ne, gt, gte, lt, lte, in, nin, not))

  def sortOrder(ascending: Boolean): MongoSortOrder[E] =
    MongoSortOrder(this -> ascending)

  def ascendingSortOrder: MongoSortOrder[E] = sortOrder(true)

  def descendingSortOrder: MongoSortOrder[E] = sortOrder(false)

  def propertyPath: String = this match {
    case FieldRef(_: MongoDataRef[_, _], fieldName, _) =>
      KeyEscaper.escape(fieldName)

    case FieldRef(prefix: MongoPropertyRef[_, _], fieldName, _) =>
      prefix.propertyPath + "." + KeyEscaper.escape(fieldName)

    case PropertyAsSubtype(ref, _, _, _) =>
      ref.propertyPath
  }

  def toProjectionBson: BsonDocument =
    Bson.document(propertyPath, Bson.int(1))

  def decode(doc: BsonDocument): T = {
    val bsonValue = propertyPath.split('.').toList.foldLeft(doc: BsonValue) {
      case (doc: BsonDocument, key) => doc.get(key)
      case _ => null
    }
    bsonValue match {
      case null => throw new NoSuchElementException(s"path $propertyPath not found in BSON document")
      case _ => BsonValueInput.read(bsonValue)(format.codec)
    }
  }
}
object MongoPropertyRef {
  implicit class IterableRefOps[C[X] <: Iterable[X], E, T](private val ref: MongoPropertyRef[E, C[T]]) extends AnyVal {
    def elementFormat: MongoFormat[T] =
      ref.format.assumeCollection.elementFormat

    def size(size: Int): MongoDocumentFilter[E] =
      ref.satisfies(ref.filterCreator.size(size))

    def elemMatch(filter: MongoFilter.Creator[T] => MongoFilter[T]): MongoDocumentFilter[E] =
      elemMatch(filter(new MongoFilter.Creator[T](elementFormat)))

    def elemMatch(filter: MongoFilter[T]): MongoDocumentFilter[E] =
      ref.satisfies(ref.filterCreator.elemMatch(filter))

    def all(values: T*): MongoDocumentFilter[E] =
      all(values)

    def all(values: Iterable[T]): MongoDocumentFilter[E] =
      ref.satisfies(ref.filterCreator.all(values))

    def collectionValueSatisfies(
      exists: OptArg[Boolean] = OptArg.Empty,
      bsonType: OptArg[BsonType] = OptArg.Empty,
      eq: OptArg[C[T]] = OptArg.Empty,
      ne: OptArg[C[T]] = OptArg.Empty,
      in: OptArg[Iterable[C[T]]] = OptArg.Empty,
      nin: OptArg[Iterable[C[T]]] = OptArg.Empty,
      not: OptArg[MongoOperatorsFilter[C[T]]] = OptArg.Empty,
      size: OptArg[Int] = OptArg.Empty,
      elemMatch: OptArg[MongoFilter[T]] = OptArg.Empty,
      all: OptArg[Iterable[T]] = OptArg.Empty,
    ): MongoDocumentFilter[E] =
      ref.satisfies(ref.filterCreator.collectionSatisfies(exists, bsonType, eq, ne, in, nin, not, size, elemMatch, all))
  }
}

object MongoRef {
  // Deliberately not calling this IdentityRef so that it doesn't get confused with IdRef (for database ID field)
  final case class SelfRef[T](
    format: MongoAdtFormat[T]
  ) extends MongoDataRef[T, T] {
    def fullFormat: MongoAdtFormat[T] = format
    def impliedFilter: MongoDocumentFilter[T] = MongoDocumentFilter.Empty()
  }

  final case class SelfAsSubtype[E, T <: E](
    fullFormat: MongoAdtFormat[E],
    caseFieldName: String,
    caseNames: List[String],
    format: MongoAdtFormat[T],
  ) extends MongoDataRef[E, T] {
    def impliedFilter: MongoDocumentFilter[E] =
      MongoDocumentFilter.subtypeFilter(this, caseFieldName, caseNames)
  }

  final case class FieldRef[E, E0, T](
    prefix: MongoRef[E, E0],
    fieldName: String,
    format: MongoFormat[T]
  ) extends MongoPropertyRef[E, T] {
    def impliedFilter: MongoDocumentFilter[E] = MongoDocumentFilter.Empty()
  }

  final case class PropertyAsSubtype[E, T0, T <: T0](
    ref: MongoPropertyRef[E, T0],
    caseFieldName: String,
    caseNames: List[String],
    format: MongoAdtFormat[T]
  ) extends MongoPropertyRef[E, T] {
    def impliedFilter: MongoDocumentFilter[E] =
      ref.impliedFilter && MongoDocumentFilter.subtypeFilter(ref, caseFieldName, caseNames)
  }

  def caseNameRef[E, T](prefix: MongoRef[E, T], caseFieldName: String): MongoPropertyRef[E, String] =
    FieldRef(prefix, caseFieldName, MongoFormat[String])
}
