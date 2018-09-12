package com.avsystem.commons
package rest.openapi

import java.util.UUID

import com.avsystem.commons.meta._
import com.avsystem.commons.misc.{MacroGenerated, NamedEnum, NamedEnumCompanion, Timestamp, ValueOf}
import com.avsystem.commons.rest.HttpBody
import com.avsystem.commons.serialization._

import scala.annotation.implicitNotFound

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
        case union: RestUnion[T] => union.createSchema(resolver)
        case record: RestRecord[T] => record.createSchema(resolver, Opt.Empty)
        case singleton: RestSingleton[T] => singleton.createSchema(resolver, Opt.Empty)
      }
      def name: Opt[String] = restStructure match {
        case _: RestSingleton[_] => Opt.Empty
        case s => s.info.rawName.opt
      }
    }
  }
}

case class RestUnion[T](
  @multi @reifyAnnot schemaAdjusters: List[SchemaAdjuster],
  @adtCaseMetadata @multi cases: List[RestCase[_]],
  @composite info: GenUnionInfo[T]
) extends RestStructure[T] {

  def createSchema(resolver: SchemaResolver): RefOr[Schema] = {
    val caseFieldOpt = info.flatten.map(_.caseFieldName)
    val caseSchemas = caseFieldOpt match {
      case Opt(caseFieldName) => cases.map { cs =>
        val caseName = cs.info.rawName
        val caseRestSchema = cs match {
          case record: RestRecord[_] => RestSchema.create(record.createSchema(_, caseFieldOpt), caseName)
          case singleton: RestSingleton[_] => RestSchema.create(singleton.createSchema(_, caseFieldOpt), caseName)
          case custom: RestCustomCase[_] =>
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
          case record: RestRecord[_] => record.createSchema(resolver, Opt.Empty)
          case singleton: RestSingleton[_] => singleton.createSchema(resolver, Opt.Empty)
          case custom: RestCustomCase[_] => resolver.resolve(custom.restSchema)
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
        case custom: RestCustomCase[_] if custom.taggedName != custom.info.rawName =>
          (custom.info.rawName, custom.taggedName)
      }.toMap
      Discriminator(caseFieldName, mapping)
    }
    RefOr(applyAdjusters(Schema(oneOf = caseSchemas, discriminator = disc.toOptArg)))
  }
}
object RestUnion extends AdtMetadataCompanion[RestUnion]

sealed trait RestCase[T] extends TypedMetadata[T] {
  def info: GenCaseInfo[T]
}
object RestCase extends AdtMetadataCompanion[RestCase]

/**
  * Will be inferred for case types that already have [[RestSchema]] defined directly.
  */
case class RestCustomCase[T](
  @checked @infer restSchema: RestSchema[T],
  @composite info: GenCaseInfo[T],
) extends RestCase[T] {
  def taggedName: String =
    if (restSchema.name.contains(info.rawName)) s"tagged${info.rawName}"
    else info.rawName
}

/**
  * Will be inferred for types having apply/unapply(Seq) pair in their companion.
  */
case class RestRecord[T](
  @multi @reifyAnnot schemaAdjusters: List[SchemaAdjuster],
  @adtParamMetadata @multi fields: List[RestField[_]],
  @composite info: GenCaseInfo[T],
) extends RestStructure[T] with RestCase[T] {

  def createSchema(resolver: SchemaResolver, caseFieldName: Opt[String]): RefOr[Schema] =
    (fields, caseFieldName) match {
      case (single :: Nil, Opt.Empty) if info.transparent =>
        resolver.resolve(single.restSchema)
      case _ =>
        val props = caseFieldName.map(cfn => (cfn, RefOr(Schema.enumOf(List(info.rawName))))).iterator ++
          fields.iterator.map(f => (f.info.rawName, resolver.resolve(f.restSchema)))
        val required = caseFieldName.iterator ++
          fields.iterator.filterNot(_.info.hasFallbackValue).map(_.info.rawName)
        RefOr(applyAdjusters(Schema(`type` = DataType.Object, properties = props.toMap, required = required.toList)))
    }
}
object RestRecord extends AdtMetadataCompanion[RestRecord]

/**
  * Will be inferred for singleton types (objects).
  */
case class RestSingleton[T](
  @multi @reifyAnnot schemaAdjusters: List[SchemaAdjuster],
  @infer @checked value: ValueOf[T],
  @composite info: GenCaseInfo[T],
) extends RestStructure[T] with RestCase[T] {

  def createSchema(resolver: SchemaResolver, caseFieldName: Opt[String]): RefOr[Schema] =
    RefOr(applyAdjusters(Schema(`type` = DataType.Object,
      properties = caseFieldName.map(cfn => (cfn, RefOr(Schema.enumOf(List(info.rawName))))).toMap,
      required = caseFieldName.toList
    )))
}
object RestSingleton extends AdtMetadataCompanion[RestSingleton]

case class RestField[T](
  @composite info: GenParamInfo[T],
  @infer restSchema: RestSchema[T]
) extends TypedMetadata[T]

@implicitNotFound("RestSchema for ${T} not found. You may provide it by making companion object of ${T} " +
  "extend RestDataCompanion[${T}] (if it is a case class or sealed hierarchy).")
trait RestSchema[T] { self =>
  def createSchema(resolver: SchemaResolver): RefOr[Schema]
  def name: Opt[String]

  def map[S](fun: RefOr[Schema] => Schema, newName: OptArg[String] = OptArg.Empty): RestSchema[S] =
    RestSchema.create(resolver => RefOr(fun(resolver.resolve(self))), newName)
  def named(name: String): RestSchema[T] =
    RestSchema.create(createSchema, name)
  def unnamed: RestSchema[T] =
    RestSchema.create(createSchema)
}
object RestSchema {
  def apply[T](implicit rt: RestSchema[T]): RestSchema[T] = rt

  def create[T](creator: SchemaResolver => RefOr[Schema], schemaName: OptArg[String] = OptArg.Empty): RestSchema[T] =
    new RestSchema[T] {
      def createSchema(resolver: SchemaResolver): RefOr[Schema] = creator(resolver)
      def name: Opt[String] = schemaName.toOpt
    }

  def plain[T](schema: Schema): RestSchema[T] =
    RestSchema.create(_ => RefOr(schema))

  implicit lazy val NothingSchema: RestSchema[Nothing] =
    RestSchema.create(_ => throw new NotImplementedError("RestSchema[Nothing]"))

  implicit lazy val UnitSchema: RestSchema[Unit] = plain(Schema(nullable = true))
  implicit lazy val NullSchema: RestSchema[Null] = plain(Schema(nullable = true))
  implicit lazy val VoidSchema: RestSchema[Void] = plain(Schema(nullable = true))

  implicit lazy val BooleanSchema: RestSchema[Boolean] = plain(Schema.Boolean)
  implicit lazy val CharSchema: RestSchema[Char] = plain(Schema.Char)
  implicit lazy val ByteSchema: RestSchema[Byte] = plain(Schema.Byte)
  implicit lazy val ShortSchema: RestSchema[Short] = plain(Schema.Short)
  implicit lazy val IntSchema: RestSchema[Int] = plain(Schema.Int)
  implicit lazy val LongSchema: RestSchema[Long] = plain(Schema.Long)
  implicit lazy val FloatSchema: RestSchema[Float] = plain(Schema.Float)
  implicit lazy val DoubleSchema: RestSchema[Double] = plain(Schema.Double)
  implicit lazy val BigIntSchema: RestSchema[BigInt] = plain(Schema.Integer)
  implicit lazy val BigDecimalSchema: RestSchema[BigDecimal] = plain(Schema.Number)

  implicit lazy val JBooleanSchema: RestSchema[JBoolean] = plain(Schema.Boolean.copy(nullable = true))
  implicit lazy val JCharacterSchema: RestSchema[JCharacter] = plain(Schema.Char.copy(nullable = true))
  implicit lazy val JByteSchema: RestSchema[JByte] = plain(Schema.Byte.copy(nullable = true))
  implicit lazy val JShortSchema: RestSchema[JShort] = plain(Schema.Short.copy(nullable = true))
  implicit lazy val JIntegerSchema: RestSchema[JInteger] = plain(Schema.Int.copy(nullable = true))
  implicit lazy val JLongSchema: RestSchema[JLong] = plain(Schema.Long.copy(nullable = true))
  implicit lazy val JFloatSchema: RestSchema[JFloat] = plain(Schema.Float.copy(nullable = true))
  implicit lazy val JDoubleSchema: RestSchema[JDouble] = plain(Schema.Double.copy(nullable = true))
  implicit lazy val JBigIntegerSchema: RestSchema[JBigInteger] = plain(Schema.Integer)
  implicit lazy val JBigDecimalSchema: RestSchema[JBigDecimal] = plain(Schema.Number)

  implicit lazy val TimestampSchema: RestSchema[Timestamp] = plain(Schema.DateTime)
  implicit lazy val JDateSchema: RestSchema[JDate] = plain(Schema.DateTime)
  implicit lazy val StringSchema: RestSchema[String] = plain(Schema.String)
  implicit lazy val SymbolSchema: RestSchema[Symbol] = plain(Schema.String)
  implicit lazy val UuidSchema: RestSchema[UUID] = plain(Schema.Uuid)

  implicit def arraySchema[T: RestSchema]: RestSchema[Array[T]] =
    RestSchema[T].map(Schema.arrayOf(_))
  implicit def seqSchema[C[X] <: BSeq[X], T: RestSchema]: RestSchema[C[T]] =
    RestSchema[T].map(Schema.arrayOf(_))
  implicit def setSchema[C[X] <: BSet[X], T: RestSchema]: RestSchema[C[T]] =
    RestSchema[T].map(Schema.arrayOf(_, uniqueItems = true))
  implicit def jCollectionSchema[C[X] <: JCollection[X], T: RestSchema]: RestSchema[C[T]] =
    RestSchema[T].map(Schema.arrayOf(_))
  implicit def jSetSchema[C[X] <: JSet[X], T: RestSchema]: RestSchema[C[T]] =
    RestSchema[T].map(Schema.arrayOf(_, uniqueItems = true))
  implicit def mapSchema[M[X, Y] <: BMap[X, Y], K, V: RestSchema]: RestSchema[M[K, V]] =
    RestSchema[V].map(Schema.mapOf)
  implicit def jMapSchema[M[X, Y] <: JMap[X, Y], K, V: RestSchema]: RestSchema[M[K, V]] =
    RestSchema[V].map(Schema.mapOf)

  implicit def optionSchema[T: RestSchema]: RestSchema[Option[T]] =
    RestSchema[T].map(Schema.nullable)
  implicit def optSchema[T: RestSchema]: RestSchema[Opt[T]] =
    RestSchema[T].map(Schema.nullable)
  implicit def optArgSchema[T: RestSchema]: RestSchema[OptArg[T]] =
    RestSchema[T].map(Schema.nullable)
  implicit def optRefSchema[T >: Null : RestSchema]: RestSchema[OptRef[T]] =
    RestSchema[T].map(Schema.nullable)
  implicit def nOptSchema[T: RestSchema]: RestSchema[NOpt[T]] =
    RestSchema[T].map(Schema.nullable)

  implicit def namedEnumSchema[E <: NamedEnum](implicit comp: NamedEnumCompanion[E]): RestSchema[E] =
    RestSchema.plain(Schema.enumOf(comp.values.iterator.map(_.name).toList))
  implicit def jEnumSchema[E <: Enum[E]](implicit ct: ClassTag[E]): RestSchema[E] =
    RestSchema.plain(Schema.enumOf(ct.runtimeClass.getEnumConstants.iterator.map(_.asInstanceOf[E].name).toList))
}

abstract class RestDataCompanion[T](
  implicit macroRestStructure: MacroGenerated[RestStructure[T]], macroCodec: MacroGenerated[GenCodec[T]]
) extends HasGenCodec[T] {
  implicit lazy val restStructure: RestStructure[T] = macroRestStructure.forCompanion(this)
  implicit lazy val restSchema: RestSchema[T] = restStructure.standaloneSchema // lazy on restStructure
}

@implicitNotFound("HttpResponseType for ${T} not found. It may be provided by appropriate RestSchema or " +
  "RestResponses instance (e.g. RestSchema[T] implies RestResponses[T] which implies HttpResponseType[Future[T]])")
case class HttpResponseType[T](responses: SchemaResolver => Responses)
object HttpResponseType {
  implicit def forFuture[T: RestResponses]: HttpResponseType[Future[T]] =
    HttpResponseType[Future[T]](RestResponses[T].responses)
}

@implicitNotFound("RestResponses for ${T} not found. You may provide it by defining an instance of RestSchema[${T}]")
case class RestResponses[T](responses: SchemaResolver => Responses)
object RestResponses {
  def apply[T](implicit r: RestResponses[T]): RestResponses[T] = r

  implicit val emptyResponseForUnit: RestResponses[Unit] =
    RestResponses(_ => Responses(byStatusCode = Map(
      204 -> RefOr(Response())
    )))

  implicit def fromSchema[T: RestSchema]: RestResponses[T] =
    RestResponses(resolver => Responses(byStatusCode = Map(
      200 -> RefOr(Response(content = Map(
        HttpBody.JsonType -> MediaType(schema = resolver.resolve(RestSchema[T])))
      ))
    )))
}

@implicitNotFound("RestRequestBody for ${T} not found. You may provide it by defining an instance of RestSchema[${T}]")
case class RestRequestBody[T](requestBody: SchemaResolver => RefOr[RequestBody])
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
    RestRequestBody(resolver => RefOr(jsonRequestBody(resolver.resolve(RestSchema[T]))))
}

trait SchemaResolver {
  def resolve(schema: RestSchema[_]): RefOr[Schema]
}

final class InliningResolver extends SchemaResolver {
  private[this] val resolving = new MHashSet[String]

  def resolve(schema: RestSchema[_]): RefOr[Schema] =
    try {
      schema.name.foreach { n =>
        if (!resolving.add(n)) {
          throw new IllegalArgumentException(s"Recursive schema reference: $n")
        }
      }
      schema.createSchema(this)
    }
    finally {
      schema.name.foreach(resolving.remove)
    }
}

final class SchemaRegistry(nameToRef: String => String, initial: Iterable[(String, RefOr[Schema])] = Map.empty)
  extends SchemaResolver {

  private[this] val registry = new MLinkedHashMap[String, RefOr[Schema]].setup(_ ++= initial)

  def registeredSchemas: Map[String, RefOr[Schema]] = registry.toMap

  def resolve(schema: RestSchema[_]): RefOr[Schema] = schema.name match {
    case Opt.Empty => schema.createSchema(this)
    case Opt(name) =>
      val ref = RefOr.ref(nameToRef(name))
      if (!registry.contains(name)) {
        registry(name) = ref // in order to handle recursive schemas - schema created by creator may refer to itself
        registry(name) = schema.createSchema(this)
      }
      ref
  }
}