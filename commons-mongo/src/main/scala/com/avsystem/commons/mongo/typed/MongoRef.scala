package com.avsystem.commons
package mongo.typed

import com.avsystem.commons.annotation.macroPrivate
import com.avsystem.commons.meta.OptionLike
import com.avsystem.commons.mongo.{BsonValueInput, KeyEscaper}
import com.avsystem.commons.serialization.GenCodec.ReadFailure
import org.bson.{BsonDocument, BsonValue}

import scala.annotation.tailrec

sealed trait MongoRef[E, T] extends MongoProjection[E, T] with DataRefDsl[E, T] { self =>
  def format: MongoFormat[T]
  def showRecordId: Boolean = false

  @macroPrivate def subtypeRefFor[C <: T : ClassTag]: ThisRef[C]

  @macroPrivate def fieldRefFor[T0](scalaFieldName: String): MongoPropertyRef[E, T0] =
    format.assumeAdt.fieldRefFor(this, scalaFieldName)

  @macroPrivate def subtypeConditionFor[C <: T : ClassTag]: MongoDocumentFilter[E] =
    format.assumeUnion.subtypeConditionFor(this, classTag[C].runtimeClass.asInstanceOf[Class[C]])
}

sealed trait MongoDataRef[E, T <: E] extends MongoRef[E, T] {
  type ThisRef[C <: T] = MongoDataRef[E, C]

  protected def thisRef: ThisRef[T] = this

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

  type ThisRef[T0 <: T] = MongoPropertyRef[E, T0]

  protected def thisRef: ThisRef[T] = this

  @macroPrivate def subtypeRefFor[C <: T : ClassTag]: MongoPropertyRef[E, C] =
    format.assumeUnion.subtypeRefFor(this, classTag[C].runtimeClass.asInstanceOf[Class[C]])

  protected def wrapQueryOperator(operator: MongoQueryOperator[T]): MongoDocumentFilter[E] =
    satisfiesFilter(MongoOperatorsFilter(Seq(operator)))

  protected def wrapUpdateOperator(operator: MongoUpdateOperator[T]): MongoUpdate[E] =
    MongoUpdate.PropertyUpdate(this, operator)

  def satisfiesFilter(filter: MongoFilter[T]): MongoDocumentFilter[E] =
    impliedFilter && MongoDocumentFilter.PropertyValueFilter(this, filter)

  def satisfies(filter: MongoFilter.Creator[T] => MongoFilter[T]): MongoDocumentFilter[E] =
    satisfiesFilter(filter(new MongoFilter.Creator[T](format)))

  def satisfiesOperators(operators: MongoQueryOperator.Creator[T] => Seq[MongoQueryOperator[T]]): MongoDocumentFilter[E] =
    satisfies(_.satisfiesOperators(operators))

  def order(ascending: Boolean): MongoDocumentOrder[E] = MongoDocumentOrder(this -> ascending)
  def ascending: MongoDocumentOrder[E] = order(true)
  def descending: MongoDocumentOrder[E] = order(false)

  def index(indexType: MongoIndexType): MongoIndex[E] = MongoIndex(this -> indexType)
  def ascendingIndex: MongoIndex[E] = index(MongoIndexType.Ascending)
  def descendingIndex: MongoIndex[E] = index(MongoIndexType.Descending)
  def hashedIndex: MongoIndex[E] = index(MongoIndexType.Hashed)
  def textIndex: MongoIndex[E] = index(MongoIndexType.Text)
  def twoDimIndex: MongoIndex[E] = index(MongoIndexType.TwoDim)
  def twoDimSphereIndex: MongoIndex[E] = index(MongoIndexType.TwoDimSphere)

  lazy val propertyPath: List[String] = {
    @tailrec def loop[T0](ref: MongoPropertyRef[E, T0], acc: List[String]): List[String] = ref match {
      case FieldRef(_: MongoDataRef[_, _], fieldName, _, _) =>
        KeyEscaper.escape(fieldName) :: acc

      case FieldRef(prefix: MongoPropertyRef[E, _], fieldName, _, _) =>
        loop(prefix, KeyEscaper.escape(fieldName) :: acc)

      case ArrayIndexRef(prefix, index, _) =>
        loop(prefix, index.toString :: acc)

      case GetFromOptional(prefix, _, _) =>
        loop(prefix, acc)

      case PropertyAsSubtype(prefix, _, _, _) =>
        loop(prefix, acc)
    }
    loop(this, Nil)
  }

  lazy val propertyPathString: String =
    propertyPath.mkString(".")

  def projectionPaths: Opt[Set[String]] =
    Opt(Set(propertyPathString))

  private def notFound =
    throw new ReadFailure(s"path $propertyPathString absent in incoming document")

  private def extractBson(doc: BsonDocument): BsonValue = this match {
    case FieldRef(_: MongoDataRef[_, _], fieldName, _, fallback) =>
      doc.get(KeyEscaper.escape(fieldName)).opt.orElse(fallback).getOrElse(notFound)

    case FieldRef(prefix: MongoPropertyRef[E, _], fieldName, _, fallback) =>
      prefix.extractBson(doc).asDocument.get(KeyEscaper.escape(fieldName)).opt.orElse(fallback).getOrElse(notFound)

    case ArrayIndexRef(prefix, index, _) =>
      val array = prefix.extractBson(doc).asArray
      if (index < array.size) array.get(index) else notFound

    case GetFromOptional(prefix, _, _) =>
      prefix.extractBson(doc)

    case PropertyAsSubtype(prefix, _, _, _) =>
      prefix.extractBson(doc)
  }

  def decodeFrom(doc: BsonDocument): T =
    BsonValueInput.read(extractBson(doc))(format.codec)
}
object MongoPropertyRef {
  implicit class CollectionRefOps[E, C[X] <: Iterable[X], T](private val ref: MongoPropertyRef[E, C[T]]) extends AnyVal {
    def head: MongoPropertyRef[E, T] = apply(0)

    def apply(index: Int): MongoPropertyRef[E, T] =
      MongoRef.ArrayIndexRef(ref, index, ref.format.assumeCollection.elementFormat)
  }

  implicit class DictionaryRefOps[E, M[X, Y] <: BMap[X, Y], K, V](private val ref: MongoPropertyRef[E, M[K, V]]) extends AnyVal {
    def apply(key: K): MongoPropertyRef[E, V] = {
      val dictFormat = ref.format.assumeDictionary
      MongoRef.FieldRef(ref, dictFormat.keyCodec.write(key), dictFormat.valueFormat, Opt.Empty)
    }
  }

  implicit def optionalRefOps[E, O, T](ref: MongoPropertyRef[E, O])(implicit optionLike: OptionLike.Aux[O, T]): OptionalRefOps[E, O, T] =
    new OptionalRefOps[E, O, T](ref)

  class OptionalRefOps[E, O, T](private val ref: MongoPropertyRef[E, O]) extends AnyVal {
    def get: MongoPropertyRef[E, T] = {
      val format = ref.format.assumeOptional[T]
      MongoRef.GetFromOptional(ref, format.wrappedFormat, format.optionLike)
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
    format: MongoAdtFormat[T]
  ) extends MongoDataRef[E, T] {
    def impliedFilter: MongoDocumentFilter[E] =
      MongoDocumentFilter.subtypeFilter(this, caseFieldName, caseNames)
  }

  final case class FieldRef[E, E0, T](
    prefix: MongoRef[E, E0],
    fieldName: String,
    format: MongoFormat[T],
    fallbackBson: Opt[BsonValue]
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
  }

  final case class GetFromOptional[E, O, T](
    prefix: MongoPropertyRef[E, O],
    format: MongoFormat[T],
    optionLike: OptionLike.Aux[O, T]
  ) extends MongoPropertyRef[E, T] {
    def impliedFilter: MongoDocumentFilter[E] = prefix.impliedFilter && prefix.isNot(optionLike.none)
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
    FieldRef(prefix, caseFieldName, MongoFormat[String], Opt.Empty)
}
