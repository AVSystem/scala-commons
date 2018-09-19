package com.avsystem.commons
package rest.openapi

import com.avsystem.commons.annotation.positioned
import com.avsystem.commons.meta._
import com.avsystem.commons.misc.ValueOf
import com.avsystem.commons.rest.JsonValue
import com.avsystem.commons.rest.openapi.adjusters.SchemaAdjuster
import com.avsystem.commons.serialization._

sealed trait RestStructure[T] extends TypedMetadata[T] {
  def schemaAdjusters: List[SchemaAdjuster]
  def info: GenInfo[T]

  protected def applyAdjusters(schema: Schema): Schema =
    schemaAdjusters.foldRight(schema)(_ adjustSchema _)
}
object RestStructure extends AdtMetadataCompanion[RestStructure] {
  implicit class LazyRestStructureOps[T](restStructure: => RestStructure[T]) {
    def standaloneSchema: RestSchema[T] = new RestSchema[T] {
      def createSchema(resolver: SchemaResolver): RefOr[Schema] = restStructure match {
        case union: Union[T] => union.createSchema(resolver)
        case record: Record[T] => record.createSchema(resolver, Opt.Empty)
        case singleton: Singleton[T] => singleton.createSchema(resolver, Opt.Empty)
      }
      def name: Opt[String] = restStructure match {
        case _: Singleton[_] => Opt.Empty
        case s => s.info.rawName.opt
      }
    }
  }

  @positioned(positioned.here) case class Union[T](
    @multi @reifyAnnot schemaAdjusters: List[SchemaAdjuster],
    @adtCaseMetadata @multi cases: List[Case[_]],
    @composite info: GenUnionInfo[T]
  ) extends RestStructure[T] {

    def createSchema(resolver: SchemaResolver): RefOr[Schema] = {
      val caseFieldOpt = info.flatten.map(_.caseFieldName)
      val caseSchemas = caseFieldOpt match {
        case Opt(caseFieldName) => cases.map { cs =>
          val caseName = cs.info.rawName
          val caseRestSchema = cs match {
            case record: Record[_] => RestSchema.create(record.createSchema(_, caseFieldOpt), caseName)
            case singleton: Singleton[_] => RestSchema.create(singleton.createSchema(_, caseFieldOpt), caseName)
            case custom: CustomCase[_] =>
              val caseFieldSchema = RefOr(Schema.enumOf(List(caseName)))
              custom.restSchema.map({
                case RefOr.Value(caseSchema) => caseSchema.copy(
                  properties = caseSchema.properties.updated(caseFieldName, caseFieldSchema),
                  required = caseFieldName :: caseSchema.required
                )
                case ref => Schema(allOf = List(RefOr(Schema(
                  `type` = DataType.Object,
                  properties = Map(caseFieldName -> caseFieldSchema),
                  required = List(caseFieldName)
                )), ref))
              }, custom.taggedName)
          }
          resolver.resolve(caseRestSchema)
        }
        case Opt.Empty => cases.map { cs =>
          val caseName = cs.info.rawName
          val caseSchema = cs match {
            case record: Record[_] => record.createSchema(resolver, Opt.Empty)
            case singleton: Singleton[_] => singleton.createSchema(resolver, Opt.Empty)
            case custom: CustomCase[_] => resolver.resolve(custom.restSchema)
          }
          RefOr(Schema(
            `type` = DataType.Object,
            properties = Map(caseName -> caseSchema),
            required = List(caseName)
          ))
        }
      }
      val disc = caseFieldOpt.map { caseFieldName =>
        val mapping = cases.collect {
          case custom: CustomCase[_] if custom.taggedName != custom.info.rawName =>
            (custom.info.rawName, custom.taggedName)
        }.toMap
        Discriminator(caseFieldName, mapping)
      }
      RefOr(applyAdjusters(Schema(oneOf = caseSchemas, discriminator = disc.toOptArg)))
    }
  }
  object Union extends AdtMetadataCompanion[Union]

  sealed trait Case[T] extends TypedMetadata[T] {
    def info: GenCaseInfo[T]
  }
  object Case extends AdtMetadataCompanion[Case]

  /**
    * Will be inferred for case types that already have [[RestSchema]] defined directly.
    */
  @positioned(positioned.here) case class CustomCase[T](
    @checked @infer restSchema: RestSchema[T],
    @composite info: GenCaseInfo[T]
  ) extends Case[T] {
    def taggedName: String =
      if (restSchema.name.contains(info.rawName)) s"tagged${info.rawName}"
      else info.rawName
  }

  /**
    * Will be inferred for types having apply/unapply(Seq) pair in their companion.
    */
  @positioned(positioned.here) case class Record[T](
    @multi @reifyAnnot schemaAdjusters: List[SchemaAdjuster],
    @adtParamMetadata @multi fields: List[Field[_]],
    @composite info: GenCaseInfo[T]
  ) extends RestStructure[T] with Case[T] {

    def createSchema(resolver: SchemaResolver, caseFieldName: Opt[String]): RefOr[Schema] =
      (fields, caseFieldName) match {
        case (single :: Nil, Opt.Empty) if info.transparent =>
          SchemaAdjuster.adjustRef(schemaAdjusters, resolver.resolve(single.restSchema))
        case _ =>
          val props = caseFieldName.map(cfn => (cfn, RefOr(Schema.enumOf(List(info.rawName))))).iterator ++
            fields.iterator.map(f => (f.info.rawName, f.resolveSchema(resolver)))
          val required = caseFieldName.iterator ++
            fields.iterator.filterNot(_.hasFallbackValue).map(_.info.rawName)
          RefOr(applyAdjusters(Schema(`type` = DataType.Object, properties = props.toMap, required = required.toList)))
      }
  }
  object Record extends AdtMetadataCompanion[Record]

  /**
    * Will be inferred for singleton types (objects).
    */
  @positioned(positioned.here) case class Singleton[T](
    @multi @reifyAnnot schemaAdjusters: List[SchemaAdjuster],
    @infer @checked value: ValueOf[T],
    @composite info: GenCaseInfo[T]
  ) extends RestStructure[T] with Case[T] {

    def createSchema(resolver: SchemaResolver, caseFieldName: Opt[String]): RefOr[Schema] =
      RefOr(applyAdjusters(Schema(`type` = DataType.Object,
        properties = caseFieldName.map(cfn => (cfn, RefOr(Schema.enumOf(List(info.rawName))))).toMap,
        required = caseFieldName.toList
      )))
  }
  object Singleton extends AdtMetadataCompanion[Singleton]

  case class Field[T](
    @composite info: GenParamInfo[T],
    @infer restSchema: RestSchema[T],
    @optional @reifyEncodedAnnot[whenAbsent[T]] whenAbsent: Opt[Try[JsonValue]],
    @multi @reifyAnnot schemaAdjusters: List[SchemaAdjuster]
  ) extends TypedMetadata[T] {
    val hasFallbackValue: Boolean =
      whenAbsent.fold(info.flags.hasDefaultValue)(_.isSuccess)

    def resolveSchema(resolver: SchemaResolver): RefOr[Schema] = {
      val bareSchema = resolver.resolve(restSchema).withDefaultValue(whenAbsent)
      SchemaAdjuster.adjustRef(schemaAdjusters, bareSchema)
    }
  }
}
