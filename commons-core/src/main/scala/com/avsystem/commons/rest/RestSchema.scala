package com.avsystem.commons
package rest

import java.util.UUID

import com.avsystem.commons.meta._
import com.avsystem.commons.misc.{MacroGenerated, NamedEnum, NamedEnumCompanion, Timestamp, ValueOf}
import com.avsystem.commons.rest.openapi._
import com.avsystem.commons.serialization._

import scala.annotation.implicitNotFound

sealed trait RestStructure[T] extends TypedMetadata[T] {
  def info: GenInfo[T]
  def schema: RefOr[Schema]
}
object RestStructure extends AdtMetadataCompanion[RestStructure]

case class RestUnion[T](
  @adtCaseMetadata @multi cases: List[RestCase[_]],
  @composite info: GenUnionInfo[T],
) extends RestStructure[T] {

  def schema: RefOr[Schema] = {
    val caseSchemas = info.flatten.map(_.caseFieldName) match {
      case Opt(caseFieldName) => cases.map { cs =>
        val caseFieldSchema = RefOr(Schema.enumOf(List(cs.info.rawName)))
        val caseRequired = if (cs.info.defaultCase) Nil else List(caseFieldName)
        val caseSchema = cs.schema match {
          case RefOr.Value(v) => v
          case RefOr.Ref(_) => throw new IllegalArgumentException(
            "schema for case type of sealed hierarchy with @flatten must not be a reference")
        }
        caseSchema.copy(
          properties = caseSchema.properties.updated(caseFieldName, caseFieldSchema),
          required = caseRequired ::: caseSchema.required
        )
      }
      case Opt.Empty => cases.map { cs =>
        val caseName = cs.info.rawName
        Schema(
          `type` = DataType.Object,
          properties = Map(caseName -> cs.schema),
          required = List(caseName)
        )
      }
    }
    RefOr(Schema(oneOf = caseSchemas.map(RefOr(_))))
  }
}
object RestUnion extends AdtMetadataCompanion[RestUnion]

sealed trait RestCase[T] extends TypedMetadata[T] {
  def info: GenCaseInfo[T]
  def schema: RefOr[Schema]
}
object RestCase extends AdtMetadataCompanion[RestCase]

/**
  * Will be inferred for case types that already have [[RestSchema]] defined directly.
  */
case class RestCustomCase[T](
  @checked @infer restSchema: RestSchema[T],
  @composite info: GenCaseInfo[T],
) extends RestCase[T] {
  def schema: RefOr[Schema] = restSchema.schema
}

/**
  * Will be inferred for types having apply/unapply(Seq) pair in their companion.
  */
case class RestRecord[T](
  @adtParamMetadata @multi fields: List[RestField[_]],
  @composite info: GenCaseInfo[T],
) extends RestStructure[T] with RestCase[T] {

  def schema: RefOr[Schema] = fields match {
    case single :: Nil if info.transparent =>
      single.restSchema.schema
    case _ =>
      val props = fields.iterator.map(f => (f.info.rawName, f.restSchema.schema)).toMap
      val required = fields.iterator.filterNot(_.info.hasFallbackValue).map(_.info.rawName).toList
      RefOr(Schema(`type` = DataType.Object, properties = props, required = required))
  }
}
object RestRecord extends AdtMetadataCompanion[RestRecord]

/**
  * Will be inferred for singleton types (objects).
  */
case class RestSingleton[T](
  @infer @checked value: ValueOf[T],
  @composite info: GenCaseInfo[T],
) extends RestStructure[T] with RestCase[T] {
  def schema: RefOr[Schema] = RefOr(Schema(`type` = DataType.Object))
}
object RestSingleton extends AdtMetadataCompanion[RestSingleton]

case class RestField[T](
  @composite info: GenParamInfo[T],
  @infer restSchema: RestSchema[T]
) extends TypedMetadata[T]

@implicitNotFound("RestSchema for ${T} not found. You may provide it by making companion object of ${T} extend RestDataCompanion[${T}] (if it is a case class or sealed hierarchy).")
trait RestSchema[T] {
  def schema: RefOr[Schema]
}
object RestSchema {
  def apply[T](implicit rt: RestSchema[T]): RestSchema[T] = rt

  def create[T](theSchema: Schema): RestSchema[T] =
    new RestSchema[T] {
      def schema: RefOr[Schema] = RefOr(theSchema)
    }

  def createLazy[T](schemaCreator: => RefOr[Schema]): RestSchema[T] =
    new RestSchema[T] {
      lazy val schema: RefOr[Schema] = schemaCreator
    }

  implicit lazy val NothingSchema: RestSchema[Nothing] =
    createLazy(sys.error("RestSchema[Nothing]"))

  implicit lazy val UnitSchema: RestSchema[Unit] = create(Schema(nullable = true))
  implicit lazy val NullSchema: RestSchema[Null] = create(Schema(nullable = true))
  implicit lazy val VoidSchema: RestSchema[Void] = create(Schema(nullable = true))

  implicit lazy val BooleanSchema: RestSchema[Boolean] = create(Schema.Boolean)
  implicit lazy val CharSchema: RestSchema[Char] = create(Schema.Char)
  implicit lazy val ByteSchema: RestSchema[Byte] = create(Schema.Byte)
  implicit lazy val ShortSchema: RestSchema[Short] = create(Schema.Short)
  implicit lazy val IntSchema: RestSchema[Int] = create(Schema.Int)
  implicit lazy val LongSchema: RestSchema[Long] = create(Schema.Long)
  implicit lazy val FloatSchema: RestSchema[Float] = create(Schema.Float)
  implicit lazy val DoubleSchema: RestSchema[Double] = create(Schema.Double)
  implicit lazy val BigIntSchema: RestSchema[BigInt] = create(Schema.Integer)
  implicit lazy val BigDecimalSchema: RestSchema[BigDecimal] = create(Schema.Number)

  implicit lazy val JBooleanSchema: RestSchema[JBoolean] = create(Schema.Boolean.copy(nullable = true))
  implicit lazy val JCharacterSchema: RestSchema[JCharacter] = create(Schema.Char.copy(nullable = true))
  implicit lazy val JByteSchema: RestSchema[JByte] = create(Schema.Byte.copy(nullable = true))
  implicit lazy val JShortSchema: RestSchema[JShort] = create(Schema.Short.copy(nullable = true))
  implicit lazy val JIntegerSchema: RestSchema[JInteger] = create(Schema.Int.copy(nullable = true))
  implicit lazy val JLongSchema: RestSchema[JLong] = create(Schema.Long.copy(nullable = true))
  implicit lazy val JFloatSchema: RestSchema[JFloat] = create(Schema.Float.copy(nullable = true))
  implicit lazy val JDoubleSchema: RestSchema[JDouble] = create(Schema.Double.copy(nullable = true))
  implicit lazy val JBigIntegerSchema: RestSchema[JBigInteger] = create(Schema.Integer)
  implicit lazy val JBigDecimalSchema: RestSchema[JBigDecimal] = create(Schema.Number)

  implicit lazy val TimestampSchema: RestSchema[Timestamp] = create(Schema.DateTime)
  implicit lazy val JDateSchema: RestSchema[JDate] = create(Schema.DateTime)
  implicit lazy val StringSchema: RestSchema[String] = create(Schema.String)
  implicit lazy val SymbolSchema: RestSchema[Symbol] = create(Schema.String)
  implicit lazy val UuidSchema: RestSchema[UUID] = create(Schema.Uuid)

  implicit def arraySchema[T: RestSchema]: RestSchema[Array[T]] =
    create(Schema.arrayOf(RestSchema[T].schema))
  implicit def seqSchema[C[X] <: BSeq[X], T: RestSchema]: RestSchema[C[T]] =
    create(Schema.arrayOf(RestSchema[T].schema))
  implicit def setSchema[C[X] <: BSet[X], T: RestSchema]: RestSchema[C[T]] =
    create(Schema.arrayOf(RestSchema[T].schema, uniqueItems = true))
  implicit def jCollectionSchema[C[X] <: JCollection[X], T: RestSchema]: RestSchema[C[T]] =
    create(Schema.arrayOf(RestSchema[T].schema))
  implicit def jSetSchema[C[X] <: JSet[X], T: RestSchema]: RestSchema[C[T]] =
    create(Schema.arrayOf(RestSchema[T].schema, uniqueItems = true))
  implicit def mapSchema[M[X, Y] <: BMap[X, Y], K, V: RestSchema]: RestSchema[M[K, V]] =
    create(Schema.mapOf(RestSchema[V].schema))
  implicit def jMapSchema[M[X, Y] <: JMap[X, Y], K, V: RestSchema]: RestSchema[M[K, V]] =
    create(Schema.mapOf(RestSchema[V].schema))

  implicit def optionSchema[T: RestSchema]: RestSchema[Option[T]] =
    RestSchema.create(Schema.nullable(RestSchema[T].schema))
  implicit def optSchema[T: RestSchema]: RestSchema[Opt[T]] =
    RestSchema.create(Schema.nullable(RestSchema[T].schema))
  implicit def optArgSchema[T: RestSchema]: RestSchema[OptArg[T]] =
    RestSchema.create(Schema.nullable(RestSchema[T].schema))
  implicit def optRefSchema[T >: Null : RestSchema]: RestSchema[OptRef[T]] =
    RestSchema.create(Schema.nullable(RestSchema[T].schema))
  implicit def nOptSchema[T: RestSchema]: RestSchema[NOpt[T]] =
    RestSchema.create(Schema.nullable(RestSchema[T].schema))

  implicit def namedEnumSchema[E <: NamedEnum](implicit comp: NamedEnumCompanion[E]): RestSchema[E] =
    RestSchema.create(Schema.enumOf(comp.values.iterator.map(_.name).toList))
  implicit def jEnumSchema[E <: Enum[E]](implicit ct: ClassTag[E]): RestSchema[E] =
    RestSchema.create(Schema.enumOf(ct.runtimeClass.getEnumConstants.iterator.map(_.asInstanceOf[E].name).toList))
}

abstract class RestDataCompanion[T](
  implicit macroRestStructure: MacroGenerated[RestStructure[T]], macroCodec: MacroGenerated[GenCodec[T]]
) extends HasGenCodec[T] {
  implicit val restStructure: RestStructure[T] = macroRestStructure.forCompanion(this)
  implicit val restSchema: RestSchema[T] = RestSchema.createLazy(restStructure.schema)
}

@implicitNotFound("RestResponses for ${T} not found. You may provide it by defining an instance of RestSchema[${T}]")
case class RestResponses[T](responses: Responses)
object RestResponses {
  def apply[T](implicit r: RestResponses[T]): RestResponses[T] = r

  implicit val emptyResponseForUnit: RestResponses[Unit] =
    RestResponses(Responses(byStatusCode = Map(
      200 -> RefOr(Response())
    )))

  implicit def fromSchema[T: RestSchema]: RestResponses[T] =
    RestResponses(Responses(byStatusCode = Map(
      200 -> RefOr(Response(content = Map(
        HttpBody.JsonType -> MediaType(schema = RestSchema[T].schema))
      ))
    )))
}

@implicitNotFound("RestRequestBody for ${T} not found. You may provide it by defining an instance of RestSchema[${T}]")
case class RestRequestBody[T](requestBody: RefOr[RequestBody])
object RestRequestBody {
  def apply[T](implicit r: RestRequestBody[T]): RestRequestBody[T] = r

  def jsonRequestBody(schema: RefOr[Schema]): RequestBody =
    RequestBody(
      content = Map(
        HttpBody.JsonType -> MediaType(schema = schema)
      ),
      required = true
    )

  implicit def fromSchema[T: RestSchema]: RestRequestBody[T] =
    RestRequestBody(RefOr(jsonRequestBody(RestSchema[T].schema)))
}
