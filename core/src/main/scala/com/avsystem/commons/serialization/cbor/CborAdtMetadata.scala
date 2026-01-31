package com.avsystem.commons
package serialization.cbor

import com.avsystem.commons.annotation.{AnnotationAggregate, positioned}
import com.avsystem.commons.meta.*
import com.avsystem.commons.serialization.*
import com.avsystem.commons.serialization.GenCodec.OOOFieldsObjectCodec

/**
 * Like [[HasGenCodec]] but generates a codec optimized for writing and reading CBOR via [[CborOutput]] and
 * [[CborInput]]. The differences between this codec and regular codec are: <ul> <li>case class fields that are `Map`s
 * are serialized with [[CborOptimizedCodecs.cborMapCodec]] so that map keys are not required to be strings - they can
 * be of arbitrary type that has [[GenCodec]]</li> <li>you can optimize CBOR for your case classes and sealed
 * hierarchies with annotations: [[cborKey]] and [[cborDiscriminator]], again taking advantage of the fact that CBOR
 * map keys can be of arbitrary type and not just strings</li> </ul>
 */
abstract class HasCborCodec[T](
  using instances: MacroInstances[CborOptimizedCodecs, (stdCodec: GenObjectCodec[T], metadata: CborAdtMetadata[T])],
) {
  given GenObjectCodec[T] = {
    val inst = instances(CborOptimizedCodecs, this)
    inst.metadata.setup(_.validate()).adjustCodec(inst.stdCodec)
  }
}

/**
 * Like [[HasCborCodec]] but allows injecting additional implicits - like [[HasGenCodecWithDeps]].
 */
abstract class HasCborCodecWithDeps[D: ValueOf, T](
  using instances: MacroInstances[(CborOptimizedCodecs, D), (stdCodec: GenObjectCodec[T], metadata: CborAdtMetadata[T])],
) {
  given GenObjectCodec[T] = {
    val inst = instances((CborOptimizedCodecs, valueOf[D]), this)
    inst.metadata.setup(_.validate()).adjustCodec(inst.stdCodec)
  }
}

/**
 * Apply this annotation on a sealed trait/class whose companion extends [[HasCborCodec]] in order to customize the
 * CBOR field key used for discriminator field. Note: this annotation automatically applies [[flatten]] annotation on
 * the sealed trait/class.
 */
class cborDiscriminator[T](discriminatorFieldKey: T, @infer codec: GenCodec[T] = infer.value[GenCodec[T]])
  extends AnnotationAggregate {
  val rawKey: RawCbor = CborOutput.writeRawCbor(discriminatorFieldKey)(using codec)

  @flatten
  final def aggregated: List[StaticAnnotation] = reifyAggregated
}

/**
 * You can apply this annotation on: <ul> <li>Fields of a case class whose companion extends [[HasCborCodec]]. This way
 * you can customize the raw CBOR key emitted for some field of that case class.</li> <li>Case classes and objects in a
 * sealed hierarchy whose companion extends [[HasCborCodec]]. This way you can customize the raw CBOR discriminator
 * value emitted for that case class/object. This applies to both nested and flat format (see [[flatten]]).</li> </ul>
 *
 * @example
 *   {{{
 *   case class Stuff(@cborKey(0) value: Int)
 *   object Stuff extends HasCborCodec[Stuff]
 *
 *   @cborDiscriminator(0) sealed trait Base
 *   @cborKey(1) case class IntCase(int: Int) extends Base
 *   @cborKey(2) case class StrCase(str: String) extends Base
 *   object Base extends HasCborCodec[Base]
 *   }}}
 */
class cborKey[T](key: T, @infer codec: GenCodec[T] = infer.value[GenCodec[T]]) extends StaticAnnotation {
  val rawKey: RawCbor = CborOutput.writeRawCbor(key)(using codec)
}

sealed trait CborAdtMetadata[T] extends TypedMetadata[T] {
  def adjustCodec(codec: GenObjectCodec[T]): GenObjectCodec[T]
  def validate(): Unit
}

object CborAdtMetadata extends AdtMetadataCompanion[CborAdtMetadata] {
  sealed trait Case[T] extends CborAdtMetadata[T] {
    def keyInfo: CborKeyInfo[T]
    def ensureUniqueKeys(discriminatorKey: Opt[RawCbor]): Unit
  }
  final class CborKeyInfo[T](
    @reifyName val sourceName: String,
    @optional @reifyAnnot val nameAnnot: Opt[name],
    @optional @reifyAnnot val cborKey: Opt[cborKey[?]],
  ) {
    val stringKey: String = nameAnnot.fold(sourceName)(_.name)
    val rawKey: RawCbor = cborKey.fold(CborOutput.writeRawCbor(stringKey))(_.rawKey)
  }
  @positioned(positioned.here)
  final class Union[T](
    @reifyName val sourceName: String,
    @optional @reifyAnnot val discriminator: Opt[cborDiscriminator[?]],
    @optional @reifyAnnot val flattenAnnot: Opt[flatten],
    @multi @adtCaseMetadata val cases: List[Case[?]],
  ) extends CborAdtMetadata[T] { union =>
    private val caseNamesKeyCodec =
      new MappingCborKeyCodec(cases.map(_.keyInfo))

    private val discriminatorKeyCodec = (discriminator `zip` flattenAnnot).map { case (disc, flatten) =>
      new DiscriminatorCborKeyCodec(flatten.caseFieldName, disc.rawKey)
    }

    def adjustCodec(codec: GenObjectCodec[T]): GenObjectCodec[T] = codec match {
      case nestedCodec: NestedSealedHierarchyCodec[T] =>
        val codecWithAdjustedCaseCodecs =
          new NestedSealedHierarchyCodec[T](
            nestedCodec.typeRepr,
            nestedCodec.caseNames,
            nestedCodec.cases,
          ) {
            def caseDependencies: Array[GenCodec[?]] =
              (nestedCodec.caseDependencies.iterator zip union.cases.iterator).map {
                case (caseCodec: ApplyUnapplyCodec[Any @unchecked], theCase: CborAdtMetadata.Record[Any @unchecked]) =>
                  theCase.adjustCodec(caseCodec)
                case (codec, _) =>
                  codec
              }.toArray
          }
        new CborRawKeysCodec(codecWithAdjustedCaseCodecs, caseNamesKeyCodec)

      case flatCodec: FlatSealedHierarchyCodec[T] =>
        val codecWithAdjustedCaseCodecs =
          new FlatSealedHierarchyCodec[T](
            flatCodec.typeRepr,
            flatCodec.caseNames,
            flatCodec.cases,
            flatCodec.oooFieldNames,
            flatCodec.caseDependentFieldNames,
            flatCodec.caseFieldName,
            flatCodec.defaultCaseIdx,
            flatCodec.defaultCaseTransient,
          ){
            override protected def doWriteCaseName(output: Output, caseName: String): Unit =
              if (!output.writeCustom(RawCbor, caseNamesKeyCodec.rawKeys(caseName))) {
                super.doWriteCaseName(output, caseName)
              }

            override protected def doReadCaseName(input: Input): String =
              input.readCustom(RawCbor).map(caseNamesKeyCodec.strKeys).getOrElse(super.doReadCaseName(input))

            def oooDependencies: Array[GenCodec[?]] = flatCodec.oooDependencies

            def caseDependencies: Array[GenCodec.OOOFieldsObjectCodec[?]] =
              (flatCodec.caseDependencies.iterator zip union.cases.iterator).map {
                case (caseCodec: ApplyUnapplyCodec[Any @unchecked], theCase: CborAdtMetadata.Record[Any @unchecked]) =>
                  theCase.adjustFlatCaseCodec(caseCodec)
                case (codec, _) =>
                  codec
              }.toArray
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
  @positioned(positioned.here)
  final class Record[T](
    @composite val keyInfo: CborKeyInfo[T],
    @multi @adtParamMetadata val fields: List[Field[?]],
  ) extends Case[T] {
    private val keyCodec = new MappingCborKeyCodec(fields.map(_.keyInfo))

    def adjustCodec(codec: GenObjectCodec[T]): GenObjectCodec[T] =
      new CborRawKeysCodec(codec, keyCodec)

    def adjustFlatCaseCodec(codec: OOOFieldsObjectCodec[T]): OOOFieldsObjectCodec[T] =
      new OOOFieldCborRawKeysCodec[T](codec, keyCodec)

    def ensureUniqueKeys(discriminatorKey: Opt[RawCbor]): Unit = {
      for {
        disc <- discriminatorKey
        field <- fields
      } {
        if (disc == field.keyInfo.rawKey) {
          throw new Exception(
            s"field ${field.keyInfo.sourceName} in case class ${keyInfo.sourceName}" +
              s" has the same CBOR key as the discriminator key",
          )
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
    @composite val keyInfo: CborKeyInfo[T],
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

  private class MappingCborKeyCodec(keyInfos: List[CborKeyInfo[?]]) extends CborKeyCodec {
    val rawKeys: Map[String, RawCbor] =
      keyInfos.mkMap(_.stringKey, _.rawKey)
    val strKeys: Map[RawCbor, String] =
      keyInfos.mkMap(_.rawKey, _.stringKey)

    def writeFieldKey(fieldName: String, output: CborOutput): Unit =
      rawKeys.getOpt(fieldName).fold(output.writeString(fieldName))(output.writeRawCbor)
    def readFieldKey(input: CborInput): String =
      input.readRawCbor() |> (cbor => strKeys.getOrElse(cbor, s"unknown_cbor_key:$cbor")) // unknown key will be ignored
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

trait CborAdtPolyInstances[C[_]] {
  def stdCodec[T: GenCodec]: GenObjectCodec[C[T]]
  def metadata[T]: CborAdtMetadata[C[T]]
}

/**
 * Like [[HasCborCodec]] but for parameterized (generic) data types.
 */
abstract class HasPolyCborCodec[C[_]](
  using instances: MacroInstances[
    CborOptimizedCodecs,
    (stdCodec: [T: GenCodec] => () => GenObjectCodec[C[T]], metadata: [T] => () => CborAdtMetadata[C[T]]),
  ],
) {
  private lazy val validatedInstances = {
    val dupa = instances(CborOptimizedCodecs, this)
    dupa.metadata[Nothing]().validate()
    dupa
  }

  given [T: GenCodec] => GenObjectCodec[C[T]] =
    validatedInstances.metadata[T]().adjustCodec(validatedInstances.stdCodec[T]())
}
