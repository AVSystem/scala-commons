package com.avsystem.commons
package mongo.model

import com.avsystem.commons.annotation.positioned
import com.avsystem.commons.macros.serialization.MongoMacros
import com.avsystem.commons.meta._
import com.avsystem.commons.misc.ValueOf
import com.avsystem.commons.mongo.BsonGenCodecs
import com.avsystem.commons.serialization._

import scala.annotation.tailrec

// TODO:
// - TransparentWrapperCompanion support
// - @generated fields support
// - multi segment paths in .ref(...)

sealed trait MongoFormat[T] {
  def codec: GenCodec[T]

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
  def fieldRefFor[E, T0](prefix: MongoRef[E, T], scalaFieldName: String): MongoRef[E, T0]
}
object MongoAdtFormat extends AdtMetadataCompanion[MongoAdtFormat] {
  @positioned(positioned.here)
  case class Union[T](
    @composite info: GenUnionInfo[T],
    @infer codec: GenCodec[T],
    @reifyAnnot flattenAnnot: flatten,
    @multi @adtCaseMetadata cases: List[Case[_]],
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
      casesPerClass.valuesIterator.map { case (subUnion: SealedParent[p], cases) =>
        (subUnion.classTag.runtimeClass, Union(subUnion.info, codec.asInstanceOf[GenCodec[p]], flattenAnnot, cases.result()))
      }.toMap
    }

    def fieldRefFor[E, T0](prefix: MongoRef[E, T], scalaFieldName: String): MongoRef[E, T0] = {
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

    private def subtypeInfo[T0](subclass: Class[T0]): (List[String], MongoFormat[T0]) = {
      def asAdtFormat[C](cse: Case[_], codec: GenCodec[_]): MongoAdtFormat[C] =
        cse.asInstanceOf[Case[C]].asAdtFormat(codec.asInstanceOf[GenCodec[C]])

      casesByClass.getOpt(subclass).map(c => (List(c.info.rawName), asAdtFormat[T0](c, codec)))
        .orElse(subUnionsByClass.getOpt(subclass).map(u => (u.cases.map(_.info.rawName), u.asInstanceOf[Union[T0]])))
        .getOrElse(throw new NoSuchElementException(s"unrecognized subclass: $subclass"))
    }

    def caseRefFor[E, T0 <: T](prefix: MongoRef[E, T], subclass: Class[T0]): MongoRef[E, T0] = {
      val (caseNames, format) = subtypeInfo(subclass)
      MongoRef.AsSubtype(prefix, flattenAnnot.caseFieldName, caseNames, format)
    }

    def caseConditionFor[E, T0 <: T](prefix: MongoRef[E, T], subclass: Class[T0]): MongoCondition[E] = {
      val (caseNames, _) = subtypeInfo(subclass)
      MongoCondition.IsSubtype(prefix, flattenAnnot.caseFieldName, caseNames)
    }
  }

  // this class exists only because I can't put GenCodec into Record/Singleton when they are used as Union cases
  // because GenCodec may not be declared explicitly for case classes in a sealed hierarchy
  @positioned(positioned.here)
  case class Record[T](
    @composite record: RecordCase[T],
    @infer codec: GenCodec[T]
  ) extends MongoAdtFormat[T] {
    def fieldRefFor[E, T0](prefix: MongoRef[E, T], scalaFieldName: String): MongoRef[E, T0] =
      record.fieldRefFor(prefix, scalaFieldName)
  }

  @positioned(positioned.here)
  case class Singleton[T](
    @composite singleton: SingletonCase[T],
    @infer codec: GenCodec[T]
  ) extends MongoAdtFormat[T] {
    def fieldRefFor[E, T0](prefix: MongoRef[E, T], scalaFieldName: String): MongoRef[E, T0] =
      singleton.fieldRefFor(prefix, scalaFieldName)
  }

  sealed trait Case[T] extends TypedMetadata[T] {
    def info: GenCaseInfo[T]
    def classTag: ClassTag[T]
    def sealedParents: List[SealedParent[_]]

    def getField(scalaFieldName: String): Opt[Field[_]]
    def fieldRefFor[E, T0](prefix: MongoRef[E, T], scalaFieldName: String): MongoRef[E, T0]

    def asAdtFormat(codec: GenCodec[T]): MongoAdtFormat[T]
  }

  @positioned(positioned.here)
  case class RecordCase[T](
    @composite info: GenCaseInfo[T],
    @infer classTag: ClassTag[T],
    @multi @adtParamMetadata @allowOptional fields: List[Field[_]],
    @multi @adtCaseSealedParentMetadata sealedParents: List[SealedParent[_]]
  ) extends Case[T] {
    def asAdtFormat(codec: GenCodec[T]): MongoAdtFormat[T] =
      Record(this, codec)

    def transparentWrapper: Boolean =
      info.transparent && fields.size == 1

    lazy val fieldsByScalaName: Map[String, Field[_]] =
      fields.toMapBy(_.info.sourceName)

    def getField(scalaFieldName: String): Opt[Field[_]] =
      fieldsByScalaName.getOpt(scalaFieldName)

    def fieldRefFor[E, T0](prefix: MongoRef[E, T], scalaFieldName: String): MongoRef[E, T0] = {
      val field = fieldsByScalaName.getOrElse(scalaFieldName,
        throw new NoSuchElementException(s"Field $scalaFieldName not found")
      ).asInstanceOf[MongoAdtFormat.Field[T0]]
      val rawName = if (transparentWrapper) Opt.Empty else Opt(field.info.rawName)
      MongoRef.FieldRef(prefix, rawName, field.format)
    }
  }

  @positioned(positioned.here)
  case class SingletonCase[T](
    @composite info: GenCaseInfo[T],
    @infer classTag: ClassTag[T],
    @multi @adtCaseSealedParentMetadata sealedParents: List[SealedParent[_]],
    @infer @checked value: ValueOf[T],
  ) extends Case[T] {
    def asAdtFormat(codec: GenCodec[T]): MongoAdtFormat[T] =
      Singleton(this, codec)

    //TODO: @generated
    def getField(scalaFieldName: String): Opt[Field[_]] = Opt.Empty

    def fieldRefFor[E, T0](prefix: MongoRef[E, T], scalaFieldName: String): MongoRef[E, T0] =
      throw new NoSuchElementException(s"Field $scalaFieldName not found")
  }

  case class Field[T](
    @composite info: GenParamInfo[T],
    @infer format: MongoFormat[T]
  ) extends TypedMetadata[T]

  case class SealedParent[T](
    @composite info: GenUnionInfo[T],
    @infer classTag: ClassTag[T]
  ) extends TypedMetadata[T]
}

trait MongoAdtInstances[T] {
  def codec: GenCodec[T]
  def format: MongoAdtFormat[T]
}

object MongoImplicits extends BsonGenCodecs

abstract class MongoDataCompanion[E](implicit instances: MacroInstances[MongoImplicits.type, MongoAdtInstances[E]]) {
  implicit val codec: GenCodec[E] = instances(MongoImplicits, this).codec
  implicit val format: MongoAdtFormat[E] = instances(MongoImplicits, this).format

  type Ref[T] = MongoRef[E, T]

  final val SelfRef: MongoRef[E, E] = MongoRef.SelfRef(format)

  def ref[T](fun: E => T): MongoRef[E, T] = macro MongoMacros.selfRefImpl
  def as[C <: E]: MongoRef[E, C] = macro MongoMacros.selfAsSubtype[C]
}
