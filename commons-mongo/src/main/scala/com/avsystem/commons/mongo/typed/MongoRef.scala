package com.avsystem.commons
package mongo.typed

import com.avsystem.commons.annotation.macroPrivate
import com.avsystem.commons.meta.OptionLike
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

  def projectionPaths: Opt[Set[String]] = Opt.empty
  def decodeFrom(doc: BsonDocument): T = BsonValueInput.read(doc)(format.codec)
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

  def order(ascending: Boolean): MongoDocumentOrder[E] =
    MongoDocumentOrder(this -> ascending)

  def ascending: MongoDocumentOrder[E] = order(true)

  def descending: MongoDocumentOrder[E] = order(false)

  lazy val propertyPath: String = this match {
    case FieldRef(_: MongoDataRef[_, _], fieldName, _) =>
      KeyEscaper.escape(fieldName)

    case FieldRef(prefix: MongoPropertyRef[_, _], fieldName, _) =>
      prefix.propertyPath + "." + KeyEscaper.escape(fieldName)

    case ArrayIndexRef(prefix, index, _) =>
      prefix.propertyPath + "." + index

    case GetFromOptional(prefix, _, _) =>
      prefix.propertyPath

    case PropertyAsSubtype(ref, _, _, _) =>
      ref.propertyPath
  }

  def projectionPaths: Opt[Set[String]] = Opt(Set(propertyPath))

  def decodeFrom(doc: BsonDocument): T = {
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
  implicit class CollectionRefOps[E, C[X] <: Iterable[X], T](private val ref: MongoPropertyRef[E, C[T]]) extends AnyVal {
    def head: MongoPropertyRef[E, T] = apply(0)

    def apply(index: Int): MongoPropertyRef[E, T] =
      MongoRef.ArrayIndexRef(ref, index, ref.format.assumeCollection.elementFormat)
  }

  implicit class OptionalRefOps[E, O, T](ref: MongoPropertyRef[E, O])(implicit optionLike: OptionLike.Aux[O, T]) {
    def get: MongoPropertyRef[E, T] = {
      val format = ref.format.assumeOptional
      MongoRef.GetFromOptional(ref, format.wrappedFormat, optionLike)
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
    def impliedFilter: MongoDocumentFilter[E] = prefix.impliedFilter
  }

  final case class ArrayIndexRef[E, C[X] <: Iterable[X], T](
    prefix: MongoPropertyRef[E, C[T]],
    index: Int,
    format: MongoFormat[T]
  ) extends MongoPropertyRef[E, T] {
    require(index >= 0, "array index must be non-negative")

    def impliedFilter: MongoDocumentFilter[E] = prefix.impliedFilter

    // array index references are not allowed in projections, we must fetch the entire array and extract element manually
    // https://docs.mongodb.com/manual/tutorial/project-fields-from-query-results/#project-specific-array-elements-in-the-returned-array
    override def projectionPaths: Opt[Set[String]] = prefix.projectionPaths
    override def decodeFrom(doc: BsonDocument): T = prefix.decodeFrom(doc).toSeq.apply(index)
  }

  final case class GetFromOptional[E, O, T](
    prefix: MongoPropertyRef[E, O],
    format: MongoFormat[T],
    optionLike: OptionLike.Aux[O, T]
  ) extends MongoPropertyRef[E, T] {
    def impliedFilter: MongoDocumentFilter[E] = prefix.impliedFilter && prefix.isNot(optionLike.none)
    override def decodeFrom(doc: BsonDocument): T = optionLike.get(prefix.decodeFrom(doc))
  }

  final case class PropertyAsSubtype[E, T0, T <: T0](
    prefix: MongoPropertyRef[E, T0],
    caseFieldName: String,
    caseNames: List[String],
    format: MongoAdtFormat[T]
  ) extends MongoPropertyRef[E, T] {
    def impliedFilter: MongoDocumentFilter[E] =
      prefix.impliedFilter && MongoDocumentFilter.subtypeFilter(prefix, caseFieldName, caseNames)
  }

  def caseNameRef[E, T](prefix: MongoRef[E, T], caseFieldName: String): MongoPropertyRef[E, String] =
    FieldRef(prefix, caseFieldName, MongoFormat[String])
}
