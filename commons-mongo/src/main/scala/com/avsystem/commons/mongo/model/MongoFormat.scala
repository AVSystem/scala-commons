package com.avsystem.commons
package mongo.model

import com.avsystem.commons.annotation.{macroPrivate, positioned}
import com.avsystem.commons.meta._
import com.avsystem.commons.misc.ValueOf
import com.avsystem.commons.mongo.{BsonGenCodecs, BsonValueInput, BsonValueOutput, mongoId}
import com.avsystem.commons.serialization._
import org.bson.BsonValue

import scala.annotation.tailrec

sealed trait MongoFormat[T] {
  implicit def codec: GenCodec[T]

  def writeBson(value: T): BsonValue =
    BsonValueOutput.write(value)

  def readBson(bsonValue: BsonValue): T =
    BsonValueInput.read[T](bsonValue)

  def assumeAdt: MongoAdtFormat[T] = this match {
    case adtFormat: MongoAdtFormat[T] => adtFormat
    case _ => throw new IllegalArgumentException(
      "Encountered a non-ADT MongoFormat for an ADT (case class or sealed hierarchy) - " +
        "do you have any custom implicit MongoFormat for that type?")
  }

  def assumeUnion: MongoAdtFormat.Union[T] = this match {
    case union: MongoAdtFormat.Union[T] => union
    case _ => throw new IllegalArgumentException(
      "Encountered a non-union MongoFormat for an union type (sealed hierarchy) -" +
        "do you have any custom implicit MongoFormat for that type?"
    )
  }
}
object MongoFormat extends MongoFormatLowPriority {
  def apply[T](implicit format: MongoFormat[T]): MongoFormat[T] = format

  final case class Leaf[T](codec: GenCodec[T]) extends MongoFormat[T]

  final case class Collection[C[X] <: Iterable[X], T](
    codec: GenCodec[C[T]],
    elementFormat: MongoFormat[T]
  ) extends MongoFormat[C[T]]

  final case class Optional[O, T](
    codec: GenCodec[O],
    wrappedFormat: MongoFormat[T]
  ) extends MongoFormat[O]

  implicit def collectionFormat[C[X] <: Iterable[X], T](
    implicit collectionCodec: GenCodec[C[T]], elementFormat: MongoFormat[T]
  ): MongoFormat[C[T]] = Collection(collectionCodec, elementFormat)

  implicit def optionalFormat[O, T](
    implicit optionLike: OptionLike.Aux[O, T], optionCodec: GenCodec[O], wrappedFormat: MongoFormat[T]
  ): MongoFormat[O] = Optional(optionCodec, wrappedFormat)

  implicit class collectionFormatOps[C[X] <: Iterable[X], T](private val format: MongoFormat[C[T]]) extends AnyVal {
    def assumeCollection: MongoFormat.Collection[C, T] = format match {
      case coll: Collection[C, T] => coll
      case _ => throw new IllegalArgumentException(
        "Encountered a non-collection MongoFormat for a collection type - " +
          "do you have a custom implicit MongoFormat for that type?")
    }
  }

  implicit class optionalFormatOps[O, T](private val format: MongoFormat[O]) extends AnyVal {
    def assumeOptional(implicit optionLike: OptionLike.Aux[O, T]): MongoFormat.Optional[O, T] = format match {
      case optional: Optional[O, T] => optional
      case _ => throw new IllegalArgumentException(
        "Encountered a non-optional MongoFormat for an Option-like type - " +
          "do you have a custom implicit MongoFormat for that type?")
    }
  }
}
trait MongoFormatLowPriority { this: MongoFormat.type =>
  implicit def leafFormat[T: GenCodec]: MongoFormat[T] = Leaf(GenCodec[T])
}

sealed trait MongoAdtFormat[T] extends MongoFormat[T] with TypedMetadata[T] {
  implicit def codec: GenObjectCodec[T]
  // this is not named `classTag` in order to avoid naming conflict with `com.avsystem.commons.classTag`
  implicit def dataClassTag: ClassTag[T]

  def fieldRefFor[E, T0](prefix: MongoRef[E, T], scalaFieldName: String): MongoPropertyRef[E, T0]
}
object MongoAdtFormat extends AdtMetadataCompanion[MongoAdtFormat] {
  @positioned(positioned.here)
  final class Union[T](
    @composite val info: GenUnionInfo[T],
    @infer val codec: GenObjectCodec[T],
    @infer val dataClassTag: ClassTag[T],
    @reifyAnnot val flattenAnnot: flatten,
    @multi @adtCaseMetadata val cases: List[Case[_]],
  ) extends MongoAdtFormat[T] {

    lazy val casesByClass: Map[Class[_], Case[_]] =
      cases.toMapBy(_.classTag.runtimeClass)

    lazy val subUnionsByClass: Map[Class[_], Union[_]] = {
      val casesPerClass = new MHashMap[Class[_], (SealedParent[_], MListBuffer[Case[_]])]
      for {
        cse <- cases
        subUnion <- cse.sealedParents
      } {
        val subclass = subUnion.classTag.runtimeClass
        casesPerClass.getOrElseUpdate(subclass, (subUnion, new MListBuffer)) match {
          case (_, buf) => buf += cse
        }
      }
      // using collect (not map) because apparently scalac thinks the match is not exhaustive
      casesPerClass.valuesIterator.collect {
        case (parent: SealedParent[p], cases) =>
          val subUnion = new Union(parent.info, codec.asInstanceOf[GenObjectCodec[p]], parent.classTag, flattenAnnot, cases.result())
          (parent.classTag.runtimeClass, subUnion)
      }.toMap
    }

    def fieldRefFor[E, T0](prefix: MongoRef[E, T], scalaFieldName: String): MongoPropertyRef[E, T0] = {
      @tailrec def loop(cases: List[Case[_]], rawName: Opt[String]): Unit = cases match {
        case cse :: tail =>
          val field = cse.getField(scalaFieldName).getOrElse(throw new NoSuchElementException(
            s"Field $scalaFieldName not found in at least one case class/object."
          ))
          if (rawName.exists(_ != field.info.rawName)) {
            throw new IllegalArgumentException(s"Field $scalaFieldName has different raw name across case classes")
          }
          loop(tail, field.info.rawName.opt)
        case Nil =>
      }
      loop(cases, Opt.Empty)
      cases.headOpt
        .map(_.asInstanceOf[Case[T]].fieldRefFor[E, T0](prefix, scalaFieldName))
        .getOrElse(throw new IllegalArgumentException("empty sealed hierarchy"))
    }

    private def subtypeInfo[T0](subclass: Class[T0]): (List[String], MongoAdtFormat[T0]) = {
      def asAdtFormat[C](cse: Case[_], codec: GenObjectCodec[_]): MongoAdtFormat[C] =
        cse.asInstanceOf[Case[C]].asAdtFormat(codec.asInstanceOf[GenObjectCodec[C]])

      casesByClass.getOpt(subclass).map(c => (List(c.info.rawName), asAdtFormat[T0](c, codec)))
        .orElse(subUnionsByClass.getOpt(subclass).map(u => (u.cases.map(_.info.rawName), u.asInstanceOf[Union[T0]])))
        .getOrElse(throw new NoSuchElementException(s"unrecognized subclass: $subclass"))
    }

    def subtypeRefFor[E >: T, T0 <: T](prefix: MongoDataRef[E, T], subclass: Class[T0]): MongoDataRef[E, T0] = {
      val (caseNames, format) = subtypeInfo(subclass)
      MongoRef.SelfAsSubtype[E, T0](prefix.fullFormat, flattenAnnot.caseFieldName, caseNames, format)
    }

    def subtypeRefFor[E, T0 <: T](prefix: MongoPropertyRef[E, T], subclass: Class[T0]): MongoPropertyRef[E, T0] = {
      val (caseNames, format) = subtypeInfo(subclass)
      MongoRef.PropertyAsSubtype(prefix, flattenAnnot.caseFieldName, caseNames, format)
    }

    def subtypeConditionFor[E, T0 <: T](prefix: MongoRef[E, T], subclass: Class[T0]): MongoDocumentFilter[E] = {
      val (caseNames, _) = subtypeInfo(subclass)
      MongoDocumentFilter.subtypeFilter(prefix, flattenAnnot.caseFieldName, caseNames)
    }
  }

  // this class exists only because I can't put GenCodec into Record/Singleton when they are used as Union cases
  // because GenCodec may not be declared explicitly for case classes in a sealed hierarchy
  @positioned(positioned.here)
  final class Record[T](
    @composite val record: RecordCase[T],
    @infer val codec: GenObjectCodec[T]
  ) extends MongoAdtFormat[T] {
    def dataClassTag: ClassTag[T] = record.classTag

    def fieldRefFor[E, T0](prefix: MongoRef[E, T], scalaFieldName: String): MongoPropertyRef[E, T0] =
      record.fieldRefFor(prefix, scalaFieldName)
  }

  @positioned(positioned.here)
  final class Singleton[T](
    @composite val singleton: SingletonCase[T],
    @infer val codec: GenObjectCodec[T]
  ) extends MongoAdtFormat[T] {
    def dataClassTag: ClassTag[T] = singleton.classTag

    def fieldRefFor[E, T0](prefix: MongoRef[E, T], scalaFieldName: String): MongoPropertyRef[E, T0] =
      singleton.fieldRefFor(prefix, scalaFieldName)
  }

  sealed trait Case[T] extends TypedMetadata[T] {
    def info: GenCaseInfo[T]
    def classTag: ClassTag[T]
    def sealedParents: List[SealedParent[_]]

    def getField(scalaFieldName: String): Opt[Field[_]]
    def fieldRefFor[E, T0](prefix: MongoRef[E, T], scalaFieldName: String): MongoPropertyRef[E, T0]

    def asAdtFormat(codec: GenObjectCodec[T]): MongoAdtFormat[T]
  }

  @positioned(positioned.here)
  final class RecordCase[T](
    @composite val info: GenCaseInfo[T],
    @infer val classTag: ClassTag[T],
    @multi @adtParamMetadata @allowOptional val fields: List[Field[_]],
    @multi @adtCaseSealedParentMetadata val sealedParents: List[SealedParent[_]]
  ) extends Case[T] {
    def asAdtFormat(codec: GenObjectCodec[T]): MongoAdtFormat[T] =
      new Record(this, codec)

    def transparentWrapper: Boolean =
      info.transparent && fields.size == 1

    lazy val fieldsByScalaName: Map[String, Field[_]] =
      fields.toMapBy(_.info.sourceName)

    def getField(scalaFieldName: String): Opt[Field[_]] =
      fieldsByScalaName.getOpt(scalaFieldName)

    def fieldRefFor[E, T0](prefix: MongoRef[E, T], scalaFieldName: String): MongoPropertyRef[E, T0] = {
      val field = fieldsByScalaName.getOrElse(scalaFieldName,
        throw new NoSuchElementException(s"Field $scalaFieldName not found")
      ).asInstanceOf[MongoAdtFormat.Field[T0]]
      prefix match {
        case fieldRef: MongoRef.FieldRef[E, _, T] if transparentWrapper =>
          fieldRef.copy(format = field.format)
        case _ =>
          MongoRef.FieldRef(prefix, field.info.rawName, field.format)
      }
    }
  }

  @positioned(positioned.here)
  final class SingletonCase[T](
    @composite val info: GenCaseInfo[T],
    @infer val classTag: ClassTag[T],
    @multi @adtCaseSealedParentMetadata val sealedParents: List[SealedParent[_]],
    @infer @checked val value: ValueOf[T],
  ) extends Case[T] {
    def asAdtFormat(codec: GenObjectCodec[T]): MongoAdtFormat[T] =
      new Singleton(this, codec)

    //TODO: @generated
    def getField(scalaFieldName: String): Opt[Field[_]] = Opt.Empty

    def fieldRefFor[E, T0](prefix: MongoRef[E, T], scalaFieldName: String): MongoPropertyRef[E, T0] =
      throw new NoSuchElementException(s"Field $scalaFieldName not found")
  }

  final class Field[T](
    @composite val info: GenParamInfo[T],
    @infer val format: MongoFormat[T]
  ) extends TypedMetadata[T]

  final class SealedParent[T](
    @composite val info: GenUnionInfo[T],
    @infer val classTag: ClassTag[T]
  ) extends TypedMetadata[T]
}

trait MongoAdtInstances[T] {
  def codec: GenObjectCodec[T]
  def format: MongoAdtFormat[T]
}

object MongoImplicits extends BsonGenCodecs

abstract class MongoDataCompanion[E](
  implicit instances: MacroInstances[MongoImplicits.type, MongoAdtInstances[E]]
) extends DataTypeDsl[E, E] with TupleProjections[E] {
  implicit val codec: GenObjectCodec[E] = instances(MongoImplicits, this).codec
  implicit val format: MongoAdtFormat[E] = instances(MongoImplicits, this).format

  type PropertyRef[T] = MongoPropertyRef[E, T]
  type TypeRef[C <: E] = MongoDataRef[E, C]

  final val SelfRef: TypeRef[E] = MongoRef.SelfRef(format)

  type ThisDataRef[C <: E] = MongoDataRef[E, C]
  @macroPrivate def thisDataRef: ThisDataRef[E] = SelfRef
}

sealed trait BaseMongoEntity {
  type IDType
  @mongoId def id: IDType
}

trait MongoEntity[ID] extends BaseMongoEntity {
  type IDType = ID
}
object MongoEntity {
  final val Id = "id"
}

abstract class MongoEntityCompanion[E <: BaseMongoEntity](
  implicit instances: MacroInstances[MongoImplicits.type, MongoAdtInstances[E]]
) extends MongoDataCompanion[E] {

  type ID = E#IDType

  final val IdRef: PropertyRef[ID] = format.fieldRefFor(SelfRef, MongoEntity.Id)
}
