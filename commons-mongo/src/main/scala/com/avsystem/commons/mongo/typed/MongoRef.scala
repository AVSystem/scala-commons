package com.avsystem.commons
package mongo.typed

import com.avsystem.commons.annotation.macroPrivate
import com.avsystem.commons.meta.OptionLike
import com.avsystem.commons.mongo.typed.MongoPropertyRef.Separator
import com.avsystem.commons.mongo.{BsonValueInput, KeyEscaper}
import com.avsystem.commons.serialization.GenCodec.ReadFailure
import org.bson.{BsonDocument, BsonValue}

/**
  * Represents a reference to a particular "place" in a MongoDB document. The "place" may be an actual path
  * inside the document ([[MongoPropertyRef]]) or the whole document _itself_ (you can think of it as an empty path).
  *
  * When the [[MongoRef]] points to the whole document, it may also narrow the type only to some subtype(s).
  * See the [[DataRefDsl.as]] macro for more details on narrowing.
  *
  * @tparam E the data type
  */
sealed trait MongoRef[E, T] extends MongoProjection[E, T] with DataRefDsl[E, T] { self =>
  def format: MongoFormat[T]
  def projectionRefs: Set[MongoRef[E, _]] = Set(this)
  def showRecordId: Boolean = false

  @macroPrivate def subtypeRefFor[C <: T : ClassTag]: MongoRef[E, C]

  @macroPrivate def fieldRefFor[T0](scalaFieldName: String): MongoPropertyRef[E, T0] =
    format.assumeAdt.fieldRefFor(this, scalaFieldName)

  @macroPrivate def subtypeFilterFor[C <: T : ClassTag](negated: Boolean): MongoDocumentFilter[E] =
    format.assumeUnion.subtypeFilterFor(this, classTag[C].runtimeClass.asInstanceOf[Class[C]], negated)

  /**
    * Composes this reference with another one, effectively prepending a "prefix" to this reference.
    * This is conceptually similar to composing functions using `scala.Function1.compose`.
    */
  def compose[P](prefix: MongoRef[P, E]): ThisRef[P, T]

  /**
    * Composes this reference with another one, effectively appending a "suffix" to this reference.
    * This is conceptually similar to composing functions using `scala.Function1.andThen`.
    */
  def andThen[S](suffix: MongoRef[T, S]): suffix.ThisRef[E, S] = suffix compose this

  def on[E0](ref: MongoRef[E0, E]): MongoProjection[E0, T] = compose(ref)
}

/**
  * A "reference" to a document type, possibly narrowed to some subtype.
  * A `MongoToplevelRef` can be used as a [[MongoProjection]] to indicate that we want a query to return
  * full documents. If the projection is narrowed to a subtype of the document, this implies an additional
  * filter so that only a subset of documents matching the subtype is returned.
  *
  * @tparam E the document type
  * @tparam T subtype of the document type, often equal to the document type
  */
sealed trait MongoToplevelRef[E, T <: E] extends MongoRef[E, T] {
  // no need to expose this as MongoToplevelRef, MongoRef is enough
  type ThisRef[E0, T0] = MongoRef[E0, T0]
  def SelfRef: MongoRef[E, T] = this

  def fullRef: MongoRef.RootRef[E]
  def format: MongoAdtFormat[T]

  @macroPrivate def subtypeRefFor[C <: T : ClassTag]: MongoToplevelRef[E, C] =
    format.assumeUnion.subtypeRefFor(this, classTag[C].runtimeClass.asInstanceOf[Class[C]])

  def decodeFrom(doc: BsonDocument): T = BsonValueInput.read(doc)(format.codec)
}

/**
  * Represents a path inside a MongoDB document.
  *
  * A [[MongoPropertyRef]] is usually obtained using the [[DataRefDsl.ref]] macro -
  * see its documentation for more details.
  *
  * [[MongoPropertyRef]] has a rich API so that it can be used for creating [[MongoDocumentFilter]]s, [[MongoDocumentUpdate]]s,
  * [[MongoDocumentOrder]]s and [[MongoIndex]]es.
  *
  * {{{
  *   case class MyEntity(id: String, number: Int) extends MongoEntity[MyEntity]
  *   object MyEntity extends MongoEntityCompanion[MyEntity]
  *
  *   val filter: MongoDocumentFilter[MyEntity] =
  *     MyEntity.ref(_.id).is("ID") && MyEntity.ref(_.number) > 8
  *
  *   val update: MongoUpdate[MyEntity] =
  *     MyEntity.ref(_.number).inc(5)
  *
  *   val order: MongoDocumentOrder[MyEntity] =
  *     MyEntity.ref(_.number).descending
  * }}}
  *
  * [[MongoPropertyRef]] may also be used as a [[MongoProjection]]
  * or as a part of a more complex, multi-field projection.
  *
  * @tparam E data type representing the whole document
  * @tparam T type of the value under the referenced field or path
  */
sealed trait MongoPropertyRef[E, T] extends MongoRef[E, T]
  with QueryOperatorsDsl[T, MongoDocumentFilter[E]]
  with UpdateOperatorsDsl[T, MongoDocumentUpdate[E]] {

  type ThisRef[E0, T0] = MongoPropertyRef[E0, T0]
  def SelfRef: MongoPropertyRef[E, T] = this

  import MongoRef._

  @macroPrivate def subtypeRefFor[C <: T : ClassTag]: MongoPropertyRef[E, C] =
    format.assumeUnion.subtypeRefFor(this, classTag[C].runtimeClass.asInstanceOf[Class[C]])

  protected def wrapQueryOperator(operator: MongoQueryOperator[T]): MongoDocumentFilter[E] =
    satisfiesFilter(MongoOperatorsFilter(Seq(operator)))

  protected def wrapUpdate(update: MongoUpdate[T]): MongoDocumentUpdate[E] =
    MongoUpdate.PropertyUpdate(this, update)

  private def satisfiesFilter(filter: MongoFilter[T]): MongoDocumentFilter[E] =
    MongoFilter.PropertyValueFilter(this, filter)

  /**
    * Creates a [[MongoDocumentFilter]] which applies some other filter on the value pointed by this
    * reference. This method accepts a lambda simply for syntactic convenience - the "creator" gives you all the
    * API for creating filters on the value type which is usually shorter than creating them manually.
    *
    * {{{
    *   case class MyEntity(id: String, data: InnerData) extends MongoEntity[MyEntity]
    *   object MyEntity extends MongoEntityCompanion[MyEntity]
    *
    *   case class InnerData(number: Int, text: String)
    *   object InnerData extends MongoDataCompanion[InnerData]
    *
    *   val filter: MongoDocumentFilter[MyEntity] =
    *     MyEntity.ref(_.number).satisfies(c => c.ref(_.number) > 0 && c.ref(_.text).startsWith("prefix"))
    * }}}
    */
  def satisfies(filter: MongoFilter.Creator[T] => MongoFilter[T]): MongoDocumentFilter[E] =
    satisfiesFilter(filter(new MongoFilter.Creator[T](format)))

  /**
    * Creates a filter that applies multiple query operators on this reference (which means that all the operators
    * must be satisfied). Note that every operator may be used only once and this is not validated statically
    * (a runtime error is thrown when some operator is used twice).
    *
    * {{{
    *   case class MyEntity(id: String, number: Int) extends MongoEntity[MyEntity]
    *   object MyEntity extends MongoEntityCompanion[MyEntity]
    *
    *   val filter: MongoDocumentFilter[MyEntity] =
    *     MyEntity.ref(_.number).satisfiesOperators(c => Seq(c.gte(0), c.lt(10)))
    * }}}
    *
    * The above produces a filter document that looks like this:
    *
    * {{{
    *   {"number": {"$$gte": 0, "$$lt": 10}}
    * }}}
    *
    * Note that the same can be usually achieved using logical operators, i.e.
    *
    * {{{
    *   val filter: MongoDocumentFilter[MyEntity] =
    *     MyEntity.ref(_.number) >= 0 && MyEntity.ref(_.number) < 10
    * }}}
    *
    * However, there are some places where this is not possible, e.g. when specifying a filter in
    * [[VanillaQueryOperatorsDsl.ForCollection.elemMatch elemMatch]].
    */
  def satisfiesOperators(operators: MongoQueryOperator.Creator[T] => Seq[MongoQueryOperator[T]]): MongoDocumentFilter[E] =
    satisfies(_.satisfiesOperators(operators))

  def updateWith(update: MongoUpdate.Creator[T] => MongoUpdate[T]): MongoDocumentUpdate[E] =
    MongoUpdate.PropertyUpdate(this, update(new MongoUpdate.Creator(format)))

  def rename(newRef: MongoPropertyRef[E, T]): MongoDocumentUpdate[E] = rename(newRef.filterPath)

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

  //noinspection NoTailRecursionAnnotation
  //no @tailrec because Scala 2.11 has problems with it
  private def computePath[T0](
    onlyUpToArray: Boolean,
    ref: MongoPropertyRef[E, T0],
    acc: List[String]
  ): List[String] = ref match {
    case FieldRef(_: MongoToplevelRef[_, _], fieldName, _, _) =>
      KeyEscaper.escape(fieldName) :: acc

    case FieldRef(prefix: MongoPropertyRef[E, _], fieldName, _, _) =>
      computePath(onlyUpToArray, prefix, KeyEscaper.escape(fieldName) :: acc)

    case ArrayIndexRef(prefix, index, _) =>
      val newAcc = if (onlyUpToArray) Nil else index.toString :: acc
      computePath(onlyUpToArray, prefix, newAcc)

    case GetFromOptional(prefix, _, _) =>
      computePath(onlyUpToArray, prefix, acc)

    case PropertySubtypeRef(prefix, _, _, _) =>
      computePath(onlyUpToArray, prefix, acc)
  }

  lazy val filterPath: String =
    computePath(onlyUpToArray = false, this, Nil).mkString(Separator)

  lazy val projectionPath: String =
    computePath(onlyUpToArray = true, this, Nil).mkString(Separator)

  def updatePath: String = filterPath

  private def notFound =
    throw new ReadFailure(s"path $filterPath absent in incoming document")

  private def extractBson(doc: BsonDocument): BsonValue = this match {
    case FieldRef(_: MongoToplevelRef[_, _], fieldName, _, fallback) =>
      doc.get(KeyEscaper.escape(fieldName)).opt.orElse(fallback).getOrElse(notFound)

    case FieldRef(prefix: MongoPropertyRef[E, _], fieldName, _, fallback) =>
      prefix.extractBson(doc).asDocument.get(KeyEscaper.escape(fieldName)).opt.orElse(fallback).getOrElse(notFound)

    case ArrayIndexRef(prefix, index, _) =>
      val array = prefix.extractBson(doc).asArray
      if (index < array.size) array.get(index) else notFound

    case GetFromOptional(prefix, _, _) =>
      prefix.extractBson(doc)

    case PropertySubtypeRef(prefix, _, _, _) =>
      prefix.extractBson(doc)
  }

  def decodeFrom(doc: BsonDocument): T =
    format.readBson(extractBson(doc))
}
object MongoPropertyRef {
  final val Separator = "."

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
  final case class RootRef[T](
    format: MongoAdtFormat[T]
  ) extends MongoToplevelRef[T, T] {
    def fullRef: RootRef[T] = this
    def compose[P](prefix: MongoRef[P, T]): MongoRef[P, T] = prefix
  }

  final case class RootSubtypeRef[E, T <: E](
    fullRef: RootRef[E],
    caseFieldName: String,
    caseNames: List[String],
    format: MongoAdtFormat[T]
  ) extends MongoToplevelRef[E, T] {
    def compose[P](prefix: MongoRef[P, E]): MongoRef[P, T] = prefix match {
      case _: MongoToplevelRef[P, E] =>
        // fullRef is guaranteed to be the same as prefix.fullRef
        // must cast because the compiler cannot infer the fact that E <: P in this case
        RootSubtypeRef(fullRef, caseFieldName, caseNames, format).asInstanceOf[MongoRef[P, T]]
      case ref: MongoPropertyRef[P, E] => PropertySubtypeRef(ref, caseFieldName, caseNames, format)
    }
  }

  final case class FieldRef[E, E0, T](
    prefix: MongoRef[E, E0],
    fieldName: String,
    format: MongoFormat[T],
    fallbackBson: Opt[BsonValue]
  ) extends MongoPropertyRef[E, T] {
    def compose[P](newPrefix: MongoRef[P, E]): MongoPropertyRef[P, T] =
      copy(prefix = this.prefix compose newPrefix)
  }

  final case class ArrayIndexRef[E, C[X] <: Iterable[X], T](
    prefix: MongoPropertyRef[E, C[T]],
    index: Int,
    format: MongoFormat[T]
  ) extends MongoPropertyRef[E, T] {
    require(index >= 0, "array index must be non-negative")
    def compose[P](newPrefix: MongoRef[P, E]): MongoPropertyRef[P, T] =
      copy(prefix = prefix compose newPrefix)
  }

  final case class GetFromOptional[E, O, T](
    prefix: MongoPropertyRef[E, O],
    format: MongoFormat[T],
    optionLike: OptionLike.Aux[O, T]
  ) extends MongoPropertyRef[E, T] {
    def compose[P](newPrefix: MongoRef[P, E]): MongoPropertyRef[P, T] =
      copy(prefix = prefix compose newPrefix)
  }

  final case class PropertySubtypeRef[E, T0, T <: T0](
    prefix: MongoPropertyRef[E, T0],
    caseFieldName: String,
    caseNames: List[String],
    format: MongoAdtFormat[T]
  ) extends MongoPropertyRef[E, T] {
    def compose[P](newPrefix: MongoRef[P, E]): MongoPropertyRef[P, T] =
      copy(prefix = prefix compose newPrefix)
  }

  def caseNameRef[E, T](prefix: MongoRef[E, T], caseFieldName: String): MongoPropertyRef[E, String] =
    FieldRef(prefix, caseFieldName, MongoFormat[String], Opt.Empty)
}
