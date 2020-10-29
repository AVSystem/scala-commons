package com.avsystem.commons
package mongo.typed

import com.avsystem.commons.annotation.macroPrivate
import com.avsystem.commons.mongo.{BsonValueInput, KeyEscaper}
import org.bson.{BsonDocument, BsonValue}

sealed trait MongoRef[E, T] extends MongoProjection[E, T] with DataTypeDsl[E, T] { self =>
  def format: MongoFormat[T]
  def showRecordId: Boolean = false

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

  def documentPaths: Opt[Set[String]] = Opt.empty
  def decode(doc: BsonDocument): T = BsonValueInput.read(doc)(format.codec)
}

sealed trait MongoPropertyRef[E, T] extends MongoRef[E, T]
  with QueryOperatorsDsl[T, MongoDocumentFilter[E]]
  with UpdateOperatorsDsl[T, MongoUpdate[E]] {

  import MongoRef._

  type ThisDataRef[T0 <: T] = MongoPropertyRef[E, T0]

  @macroPrivate final def thisDataRef: ThisDataRef[T] = this

  @macroPrivate def subtypeRefFor[C <: T : ClassTag]: MongoPropertyRef[E, C] =
    format.assumeUnion.subtypeRefFor(this, classTag[C].runtimeClass.asInstanceOf[Class[C]])

  protected def wrapQueryOperator(operator: MongoQueryOperator[T]): MongoDocumentFilter[E] =
    MongoDocumentFilter.PropertyValueFilter(this, MongoOperatorsFilter(Seq(operator)))

  protected def wrapUpdateOperator(operator: MongoUpdateOperator[T]): MongoUpdate[E] =
    MongoUpdate.PropertyUpdate(this, operator)

  def satisfiesFilter(filter: MongoFilter[T]): MongoDocumentFilter[E] =
    MongoDocumentFilter.PropertyValueFilter(this, filter)

  def satisfies(filter: MongoFilter.Creator[T] => MongoFilter[T]): MongoDocumentFilter[E] =
    MongoDocumentFilter.PropertyValueFilter(this, filter(new MongoFilter.Creator[T](format)))

  def satisfiesOperators(operators: MongoQueryOperator.Creator[T] => Seq[MongoQueryOperator[T]]): MongoDocumentFilter[E] =
    satisfies(_.satisfiesOperators(operators))

  def sortOrder(ascending: Boolean): MongoDocumentOrder[E] =
    MongoDocumentOrder(this -> ascending)

  def ascendingSortOrder: MongoDocumentOrder[E] = sortOrder(true)

  def descendingSortOrder: MongoDocumentOrder[E] = sortOrder(false)

  lazy val propertyPath: String = this match {
    case FieldRef(_: MongoDataRef[_, _], fieldName, _) =>
      KeyEscaper.escape(fieldName)

    case FieldRef(prefix: MongoPropertyRef[_, _], fieldName, _) =>
      prefix.propertyPath + "." + KeyEscaper.escape(fieldName)

    case PropertyAsSubtype(ref, _, _, _) =>
      ref.propertyPath
  }

  def documentPaths: Opt[Set[String]] = Opt(Set(propertyPath))

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

object MongoRef {
  // Deliberately not calling this IdentityRef so that it doesn't get confused with IdRef (for database ID field)
  final case class SelfRef[T](
    format: MongoAdtFormat[T]
  ) extends MongoDataRef[T, T] {
    def fullFormat: MongoAdtFormat[T] = format
    def impliedFilter: MongoDocumentFilter[T] = MongoDocumentFilter.empty
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
    def impliedFilter: MongoDocumentFilter[E] = MongoDocumentFilter.empty
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
