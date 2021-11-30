package com.avsystem.commons
package mongo.typed

import com.avsystem.commons.annotation.positioned
import com.avsystem.commons.meta._
import com.avsystem.commons.misc.{TypedMap, ValueOf}
import com.avsystem.commons.mongo.{BsonValueInput, BsonValueOutput}
import com.avsystem.commons.serialization._
import org.bson.{BsonNull, BsonValue}

import scala.annotation.tailrec

/**
  * Typeclass that captures internal structure of a type that can be saved to MongoDB
  * (directly as a toplevel entity or indirectly as an embedded value).
  */
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

  def assumeUnion: MongoAdtFormat.UnionFormat[T] = this match {
    case union: MongoAdtFormat.UnionFormat[T] => union
    case _ => throw new IllegalArgumentException(
      "Encountered a non-union MongoFormat for an union type (sealed hierarchy) -" +
        "do you have any custom implicit MongoFormat for that type?"
    )
  }

  def assumeOptional[W]: MongoFormat.OptionalFormat[T, W] = this match {
    case optional: MongoFormat.OptionalFormat[T@unchecked, W@unchecked] => optional
    case _ => throw new IllegalArgumentException(
      "Encountered a non-optional MongoFormat for an Option-like type - " +
        "do you have a custom implicit MongoFormat for that type?")
  }
}
object MongoFormat extends MetadataCompanion[MongoFormat] with MongoFormatLowPriority {
  final case class Opaque[T](
    codec: GenCodec[T]
  ) extends MongoFormat[T]

  final case class CollectionFormat[C[X] <: Iterable[X], T](
    codec: GenCodec[C[T]],
    elementFormat: MongoFormat[T]
  ) extends MongoFormat[C[T]]

  final case class DictionaryFormat[M[X, Y] <: BMap[X, Y], K, V](
    codec: GenCodec[M[K, V]],
    keyCodec: GenKeyCodec[K],
    valueFormat: MongoFormat[V]
  ) extends MongoFormat[M[K, V]]

  final case class TypedMapFormat[K[_]](
    codec: GenCodec[TypedMap[K]],
    keyCodec: GenKeyCodec[K[_]],
    valueFormats: MongoFormatMapping[K]
  ) extends MongoFormat[TypedMap[K]]

  final case class OptionalFormat[O, T](
    codec: GenCodec[O],
    optionLike: OptionLike.Aux[O, T],
    wrappedFormat: MongoFormat[T]
  ) extends MongoFormat[O]

  implicit def collectionFormat[C[X] <: Iterable[X], T](
    implicit collectionCodec: GenCodec[C[T]], elementFormat: MongoFormat[T]
  ): MongoFormat[C[T]] = CollectionFormat(collectionCodec, elementFormat)

  implicit def dictionaryFormat[M[X, Y] <: BMap[X, Y], K, V](
    implicit mapCodec: GenCodec[M[K, V]], keyCodec: GenKeyCodec[K], valueFormat: MongoFormat[V]
  ): MongoFormat[M[K, V]] = DictionaryFormat(mapCodec, keyCodec, valueFormat)

  implicit def typedMapFormat[K[_]](
    implicit keyCodec: GenKeyCodec[K[_]], valueFormats: MongoFormatMapping[K]
  ): MongoFormat[TypedMap[K]] = TypedMapFormat[K](TypedMap.typedMapCodec, keyCodec, valueFormats)

  implicit def optionalFormat[O, T](
    implicit optionLike: OptionLike.Aux[O, T], optionCodec: GenCodec[O], wrappedFormat: MongoFormat[T]
  ): MongoFormat[O] = OptionalFormat(optionCodec, optionLike, wrappedFormat)

  implicit class collectionFormatOps[C[X] <: Iterable[X], T](private val format: MongoFormat[C[T]]) extends AnyVal {
    def assumeCollection: CollectionFormat[C, T] = format match {
      case coll: CollectionFormat[C@unchecked, T@unchecked] => coll
      case _ => throw new IllegalArgumentException(
        "Encountered a non-collection MongoFormat for a collection type - " +
          "do you have a custom implicit MongoFormat for that type?")
    }
  }

  implicit class dictionaryFormatOps[M[X, Y] <: BMap[X, Y], K, V](private val format: MongoFormat[M[K, V]]) extends AnyVal {
    def assumeDictionary: DictionaryFormat[M, K, V] = format match {
      case dict: DictionaryFormat[M@unchecked, K@unchecked, V@unchecked] => dict
      case _ => throw new IllegalArgumentException(
        "Encountered a non-dictionary MongoFormat for a dictionary type - " +
          "do you have a custom implicit MongoFormat for that type?")
    }
  }

  implicit class typedMapFormatOps[K[_]](private val format: MongoFormat[TypedMap[K]]) extends AnyVal {
    def assumeTypedMap: TypedMapFormat[K] = format match {
      case typedMap: TypedMapFormat[K] => typedMap
      case _ => throw new IllegalArgumentException(
        "Encountered a non-typed-map MongoFormat for a TypedMap type - " +
          "do you have a custom implicit MongoFormat for that type?")
    }
  }
}
trait MongoFormatLowPriority { this: MongoFormat.type =>
  implicit def leafFormat[T: GenCodec]: MongoFormat[T] = Opaque(GenCodec[T])
}

sealed trait MongoAdtFormat[T] extends MongoFormat[T] with TypedMetadata[T] {
  implicit def codec: GenObjectCodec[T]
  // this is not named `classTag` in order to avoid naming conflict with `com.avsystem.commons.classTag`
  implicit def dataClassTag: ClassTag[T]

  def fieldRefFor[E, T0](prefix: MongoRef[E, T], scalaFieldName: String): MongoPropertyRef[E, T0]
}
object MongoAdtFormat extends AdtMetadataCompanion[MongoAdtFormat] {
  @positioned(positioned.here)
  final class UnionFormat[T](
    @composite val info: GenUnionInfo[T],
    @infer val codec: GenObjectCodec[T],
    @infer val dataClassTag: ClassTag[T],
    @reifyAnnot val flattenAnnot: flatten,
    @multi @adtCaseMetadata val cases: List[Case[_]]
  ) extends MongoAdtFormat[T] {

    lazy val casesByClass: Map[Class[_], Case[_]] =
      cases.toMapBy(_.classTag.runtimeClass)

    lazy val subUnionsByClass: Map[Class[_], UnionFormat[_]] = {
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
          val subUnion = new UnionFormat(parent.info, codec.asInstanceOf[GenObjectCodec[p]], parent.classTag, flattenAnnot, cases.result())
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
        .orElse(subUnionsByClass.getOpt(subclass).map(u => (u.cases.map(_.info.rawName), u.asInstanceOf[UnionFormat[T0]])))
        .getOrElse(throw new NoSuchElementException(s"unrecognized subclass: $subclass"))
    }

    def subtypeRefFor[E >: T, T0 <: T](prefix: MongoToplevelRef[E, T], subclass: Class[T0]): MongoToplevelRef[E, T0] = {
      val (caseNames, format) = subtypeInfo(subclass)
      MongoRef.RootSubtypeRef[E, T0](prefix.fullRef, flattenAnnot.caseFieldName, caseNames, format)
    }

    def subtypeRefFor[E, T0 <: T](prefix: MongoPropertyRef[E, T], subclass: Class[T0]): MongoPropertyRef[E, T0] = {
      val (caseNames, format) = subtypeInfo(subclass)
      MongoRef.PropertySubtypeRef(prefix, flattenAnnot.caseFieldName, caseNames, format)
    }

    def subtypeFilterFor[E, T0 <: T](prefix: MongoRef[E, T], subclass: Class[T0], negated: Boolean): MongoDocumentFilter[E] = {
      val (caseNames, _) = subtypeInfo(subclass)
      MongoFilter.subtypeFilter(prefix, flattenAnnot.caseFieldName, caseNames, negated)
    }
  }

  // this class exists only because I can't put GenCodec into Record/Singleton when they are used as Union cases
  // because GenCodec may not be declared explicitly for case classes in a sealed hierarchy
  @positioned(positioned.here)
  final class RecordFormat[T](
    @composite val record: RecordCase[T],
    @infer val codec: GenObjectCodec[T]
  ) extends MongoAdtFormat[T] {
    def dataClassTag: ClassTag[T] = record.classTag

    def fieldRefFor[E, T0](prefix: MongoRef[E, T], scalaFieldName: String): MongoPropertyRef[E, T0] =
      record.fieldRefFor(prefix, scalaFieldName)
  }

  @positioned(positioned.here)
  final class SingletonFormat[T](
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
    @multi @adtParamMetadata val fields: List[Field[_]],
    @multi @adtCaseSealedParentMetadata val sealedParents: List[SealedParent[_]]
  ) extends Case[T] {
    def asAdtFormat(codec: GenObjectCodec[T]): MongoAdtFormat[T] =
      new RecordFormat(this, codec)

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
          fieldRef.copy(format = field.format.value)
        case _ =>
          MongoRef.FieldRef(prefix, field.info.rawName, field.format.value, field.fallbackBson)
      }
    }
  }

  @positioned(positioned.here)
  final class SingletonCase[T](
    @composite val info: GenCaseInfo[T],
    @infer val classTag: ClassTag[T],
    @multi @adtCaseSealedParentMetadata val sealedParents: List[SealedParent[_]],
    @infer @checked val value: ValueOf[T]
  ) extends Case[T] {
    def asAdtFormat(codec: GenObjectCodec[T]): MongoAdtFormat[T] =
      new SingletonFormat(this, codec)

    //TODO: @generated
    def getField(scalaFieldName: String): Opt[Field[_]] = Opt.Empty

    def fieldRefFor[E, T0](prefix: MongoRef[E, T], scalaFieldName: String): MongoPropertyRef[E, T0] =
      throw new NoSuchElementException(s"Field $scalaFieldName not found")
  }

  final class Field[T](
    @composite val info: GenParamInfo[T],
    @optional @reifyDefaultValue defaultValue: Opt[DefaultValue[T]],
    @optional @reifyAnnot whenAbsentAnnot: Opt[whenAbsent[T]],
    @infer val format: MongoFormat.Lazy[T]
  ) extends TypedMetadata[T] {
    lazy val fallbackBson: Opt[BsonValue] = {
      if (info.optional) Opt(BsonNull.VALUE)
      else whenAbsentAnnot.map(a => Try(a.value)).orElse(defaultValue.map(a => Try(a.value)))
        .flatMap(_.toOpt).map(v => BsonValueOutput.write(v)(format.value.codec))
    }
  }

  final class SealedParent[T](
    @composite val info: GenUnionInfo[T],
    @infer val classTag: ClassTag[T]
  ) extends TypedMetadata[T]
}

final class MongoEntityMeta[E <: BaseMongoEntity](
  @infer val format: MongoAdtFormat[E],
  @infer val idMode: EntityIdMode[E, E#IDType]
) {
  def idRef: MongoPropertyRef[E, E#IDType] = idMode.idRef(format)
}
object MongoEntityMeta extends BoundedAdtMetadataCompanion[BaseMongoEntity, Nothing, MongoEntityMeta] {
  private[typed] def bincompatMeta[E <: BaseMongoEntity](format: MongoAdtFormat[E]): MongoEntityMeta[E] =
    new MongoEntityMeta(format, EntityIdMode.Explicit())
}
