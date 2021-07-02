package com.avsystem.commons
package serialization.cbor

import com.avsystem.commons.annotation.positioned
import com.avsystem.commons.meta._
import com.avsystem.commons.serialization.GenCodec.OOOFieldsObjectCodec
import com.avsystem.commons.serialization._

class cborDiscriminator[T](discriminatorFieldKey: T, @infer codec: GenCodec[T] = infer.value) extends StaticAnnotation {
  val rawKey: RawCbor = CborOutput.write(discriminatorFieldKey)(codec)
}

class cborKey[T](key: T, @infer codec: GenCodec[T] = infer.value) extends StaticAnnotation {
  val rawKey: RawCbor = CborOutput.write(key)(codec)
}

sealed trait CborAdtMetadata[T] extends TypedMetadata[T] {
  def adjustCodec(codec: GenObjectCodec[T]): GenObjectCodec[T]
}

object CborAdtMetadata extends AdtMetadataCompanion[CborAdtMetadata] {
  final class CborKeyInfo[T](
    @reifyName val sourceName: String,
    @optional @reifyAnnot val nameAnnot: Opt[name],
    @optional @reifyAnnot val cborKey: Opt[cborKey[_]]
  ) {
    val stringKey: String = nameAnnot.fold(sourceName)(_.name)
    val rawKey: RawCbor = cborKey.fold(CborOutput.write(stringKey))(_.rawKey)
  }

  @positioned(positioned.here)
  final class Union[T](
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
        CborCustomCodecs.cborRawKeysCodec(caseNamesKeyCodec)(codecWithAdjustedCaseCodecs)

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
          .map(CborCustomCodecs.cborRawKeysCodec(_)(codecWithAdjustedCaseCodecs))
          .getOrElse(codecWithAdjustedCaseCodecs)

      case _ =>
        codec
    }
  }

  sealed trait Case[T] extends CborAdtMetadata[T] {
    def keyInfo: CborKeyInfo[T]
  }

  @positioned(positioned.here)
  final class Record[T](
    @composite val keyInfo: CborKeyInfo[T],
    @multi @adtParamMetadata val fields: List[Field[_]]
  ) extends Case[T] {
    private val keyCodec = new MappingCborKeyCodec(fields.map(_.keyInfo))

    def adjustCodec(codec: GenObjectCodec[T]): GenObjectCodec[T] =
      CborCustomCodecs.cborRawKeysCodec(keyCodec)(codec)

    def adjustFlatCaseCodec(codec: OOOFieldsObjectCodec[T]): OOOFieldsObjectCodec[T] =
      new OOOFieldCborRawKeysCodec[T](codec, keyCodec)
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

  def cborCodec: GenObjectCodec[T] = metadata.adjustCodec(stdCodec)
}

abstract class HasCborCodec[T](implicit instances: MacroInstances[CborCustomCodecs, CborAdtInstances[T]]) {
  implicit lazy val codec: GenObjectCodec[T] = instances(CborCustomCodecs, this).cborCodec
}
