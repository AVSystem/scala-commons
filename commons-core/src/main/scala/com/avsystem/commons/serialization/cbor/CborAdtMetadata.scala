package com.avsystem.commons
package serialization.cbor

import com.avsystem.commons.annotation.{AnnotationAggregate, positioned}
import com.avsystem.commons.meta._
import com.avsystem.commons.misc.ValueOf
import com.avsystem.commons.serialization.GenCodec.OOOFieldsObjectCodec
import com.avsystem.commons.serialization._

/**
  * Apply this annotation on a sealed trait/class whose companion extends [[HasCborCodec]] in order to customize
  * the CBOR field key used for discriminator field. Note: this annotation automatically applies [[flatten]] annotation
  * on the sealed trait/class.
  */
class cborDiscriminator[T](discriminatorFieldKey: T, @infer codec: GenCodec[T] = infer.value) extends AnnotationAggregate {
  val rawKey: RawCbor = CborOutput.writeRawCbor(discriminatorFieldKey)(codec)

  @flatten
  final def aggregated: List[StaticAnnotation] = reifyAggregated
}

/**
  * You can apply this annotation on:
  * <ul>
  * <li>Fields of a case class whose companion extends [[HasCborCodec]]. This way you can customize the raw CBOR
  * key emitted for some field of that case class.</li>
  * <li>Case classes and objects in a sealed hierarchy whose companion extends [[HasCborCodec]]. This way you can
  * customize the raw CBOR discriminator value emitted for that case class/object. This applies to both
  * nested and flat format (see [[flatten]]).</li>
  * </ul>
  *
  * @example
  * {{{
  *   case class Stuff(@cborKey(0) value: Int)
  *   object Stuff extends HasCborCodec[Stuff]
  * }}}
  */
class cborKey[T](key: T, @infer codec: GenCodec[T] = infer.value) extends StaticAnnotation {
  val rawKey: RawCbor = CborOutput.writeRawCbor(key)(codec)
}

sealed trait CborAdtMetadata[T] extends TypedMetadata[T] {
  def adjustCodec(codec: GenObjectCodec[T]): GenObjectCodec[T]
  def validate(): Unit
}

object CborAdtMetadata extends AdtMetadataCompanion[CborAdtMetadata] {
  final class CborKeyInfo[T](
    @reifyName val sourceName: String,
    @optional @reifyAnnot val nameAnnot: Opt[name],
    @optional @reifyAnnot val cborKey: Opt[cborKey[_]]
  ) {
    val stringKey: String = nameAnnot.fold(sourceName)(_.name)
    val rawKey: RawCbor = cborKey.fold(CborOutput.writeRawCbor(stringKey))(_.rawKey)
  }

  @positioned(positioned.here)
  final class Union[T](
    @reifyName val sourceName: String,
    @optional @reifyAnnot val discriminator: Opt[cborDiscriminator[_]],
    @optional @reifyAnnot val flattenAnnot: Opt[flatten],
    @multi @adtCaseMetadata val cases: List[Case[_]],
  ) extends CborAdtMetadata[T] { union =>
    private val caseNamesKeyCodec =
      new MappingCborKeyCodec(cases.map(_.keyInfo))

    private val discriminatorKeyCodec =
      (discriminator zip flattenAnnot)
        .map({ case (disc, flatten) => new DiscriminatorCborKeyCodec(flatten.caseFieldName, disc.rawKey) })

    def adjustCodec(codec: GenObjectCodec[T]): GenObjectCodec[T] = codec match {
      case nestedCodec: NestedSealedHierarchyCodec[T] =>
        val codecWithAdjustedCaseCodecs =
          new NestedSealedHierarchyCodec[T](
            nestedCodec.typeRepr,
            nestedCodec.nullable,
            nestedCodec.caseNames,
            nestedCodec.cases
          ) {
            def caseDependencies: Array[GenCodec[_]] =
              (nestedCodec.caseDependencies.iterator zip union.cases.iterator)
                .map {
                  case (caseCodec: ApplyUnapplyCodec[Any@unchecked], theCase: CborAdtMetadata.Record[Any@unchecked]) =>
                    theCase.adjustCodec(caseCodec)
                  case (codec, _) =>
                    codec
                }
                .toArray
          }
        new CborRawKeysCodec(codecWithAdjustedCaseCodecs, caseNamesKeyCodec)

      case flatCodec: FlatSealedHierarchyCodec[T] =>
        val codecWithAdjustedCaseCodecs =
          new FlatSealedHierarchyCodec[T](
            flatCodec.typeRepr,
            flatCodec.nullable,
            flatCodec.caseNames,
            flatCodec.cases,
            flatCodec.oooFieldNames,
            flatCodec.caseDependentFieldNames,
            flatCodec.caseFieldName,
            flatCodec.defaultCaseIdx,
            flatCodec.defaultCaseTransient
          ) {
            override protected def doWriteCaseName(output: Output, caseName: String): Unit =
              if (!output.writeCustom(RawCbor, caseNamesKeyCodec.rawKeys(caseName))) {
                super.doWriteCaseName(output, caseName)
              }

            override protected def doReadCaseName(input: Input): String =
              input.readCustom(RawCbor).map(caseNamesKeyCodec.strKeys).getOrElse(super.doReadCaseName(input))

            def oooDependencies: Array[GenCodec[_]] = flatCodec.oooDependencies

            def caseDependencies: Array[GenCodec.OOOFieldsObjectCodec[_]] =
              (flatCodec.caseDependencies.iterator zip union.cases.iterator)
                .map {
                  case (caseCodec: ApplyUnapplyCodec[Any@unchecked], theCase: CborAdtMetadata.Record[Any@unchecked]) =>
                    theCase.adjustFlatCaseCodec(caseCodec)
                  case (codec, _) =>
                    codec
                }
                .toArray
          }

        discriminatorKeyCodec
          .map(new CborRawKeysCodec(codecWithAdjustedCaseCodecs, _))
          .getOrElse(codecWithAdjustedCaseCodecs)

      case _ =>
        codec
    }

    def validate(): Unit = {
      cases.groupBy(_.keyInfo.rawKey).foreach {
        case (_, List(_)) =>
        case (_, multipleCases) =>
          val caseNames = multipleCases.map(_.keyInfo.sourceName).mkString(", ")
          throw new Exception(s"case classes/objects $caseNames in sealed hierarchy $sourceName have the same CBOR key")
      }
      cases.foreach(_.ensureUniqueKeys(discriminator.map(_.rawKey)))
    }
  }

  sealed trait Case[T] extends CborAdtMetadata[T] {
    def keyInfo: CborKeyInfo[T]
    def ensureUniqueKeys(discriminatorKey: Opt[RawCbor]): Unit
  }

  @positioned(positioned.here)
  final class Record[T](
    @composite val keyInfo: CborKeyInfo[T],
    @multi @adtParamMetadata val fields: List[Field[_]]
  ) extends Case[T] {
    private val keyCodec = new MappingCborKeyCodec(fields.map(_.keyInfo))

    def adjustCodec(codec: GenObjectCodec[T]): GenObjectCodec[T] =
      new CborRawKeysCodec(codec, keyCodec)

    def adjustFlatCaseCodec(codec: OOOFieldsObjectCodec[T]): OOOFieldsObjectCodec[T] =
      new OOOFieldCborRawKeysCodec[T](codec, keyCodec)

    def ensureUniqueKeys(discriminatorKey: Opt[RawCbor]): Unit = {
      for (disc <- discriminatorKey; field <- fields) {
        if (disc == field.keyInfo.rawKey) {
          throw new Exception(s"field ${field.keyInfo.sourceName} in case class ${keyInfo.sourceName}" +
            s" has the same CBOR key as the discriminator key")
        }
      }
      fields.groupBy(_.keyInfo.rawKey).foreach {
        case (_, List(_)) =>
        case (_, multipleFields) =>
          val fieldNames = multipleFields.map(_.keyInfo.sourceName).mkString(", ")
          throw new Exception(s"fields $fieldNames in case class ${keyInfo.sourceName} have the same CBOR key")
      }
    }

    def validate(): Unit = ensureUniqueKeys(Opt.Empty)
  }

  final class Field[T](
    @composite val keyInfo: CborKeyInfo[T]
  ) extends TypedMetadata[T]

  @positioned(positioned.here)
  final class Singleton[T](
    @infer @checked val value: ValueOf[T],
    @composite val keyInfo: CborKeyInfo[T],
  ) extends Case[T] {
    def adjustCodec(codec: GenObjectCodec[T]): GenObjectCodec[T] = codec
    def ensureUniqueKeys(discriminatorKey: Opt[RawCbor]): Unit = ()
    def validate(): Unit = ()
  }

  private class MappingCborKeyCodec(keyInfos: List[CborKeyInfo[_]]) extends CborKeyCodec {
    val rawKeys: Map[String, RawCbor] =
      keyInfos.mkMap(_.stringKey, _.rawKey)
    val strKeys: Map[RawCbor, String] =
      keyInfos.mkMap(_.rawKey, _.stringKey)

    def writeFieldKey(fieldName: String, output: CborOutput): Unit =
      output.writeRawCbor(rawKeys(fieldName))
    def readFieldKey(input: CborInput): String =
      strKeys(input.readRawCbor())
  }

  private class DiscriminatorCborKeyCodec(strKey: String, rawDisc: RawCbor) extends CborKeyCodec {
    def writeFieldKey(fieldName: String, output: CborOutput): Unit =
      if (fieldName == strKey) output.writeRawCbor(rawDisc)
      else output.writeString(fieldName)

    def readFieldKey(input: CborInput): String = {
      val rawKey = input.readRawCbor()
      if (rawKey == rawDisc) strKey else rawKey.readAs[String]()
    }
  }
}

trait CborAdtInstances[T] {
  def stdCodec: GenObjectCodec[T]
  def metadata: CborAdtMetadata[T]

  def cborCodec: GenObjectCodec[T] =
    metadata.setup(_.validate()).adjustCodec(stdCodec)
}

abstract class HasCborCodec[T](implicit instances: MacroInstances[CborCustomCodecs, CborAdtInstances[T]]) {
  implicit lazy val codec: GenObjectCodec[T] = instances(CborCustomCodecs, this).cborCodec
}
