package com.avsystem.commons
package rest.openapi

import com.avsystem.commons.misc.{AbstractValueEnum, AbstractValueEnumCompanion, EnumCtx}
import com.avsystem.commons.rest.JsonValue
import com.avsystem.commons.serialization.json.JsonStringOutput
import com.avsystem.commons.serialization.{transientDefault => td, _}

/**
  * Representation of
  * [[https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.1.md#openapi-object OpenAPI Object]]
  * from [[https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.1.md OpenAPI 3.0 specification]].
  * It may be serialized to OpenAPI 3.0 compliant JSON using
  * [[com.avsystem.commons.serialization.json.JsonStringOutput JsonStringOutput]].
  * This JSON can then be consumed by tools that support OpenAPI 3.0, e.g.
  * [[https://swagger.io/tools/swagger-ui/ Swagger UI]].
  */
case class OpenApi(
  openapi: String = OpenApi.Version,
  info: Info,
  paths: Paths,
  @td servers: List[Server] = Nil,
  @td components: OptArg[Components] = OptArg.Empty,
  @td security: List[SecurityRequirement] = Nil,
  @td tags: List[Tag] = Nil,
  @td externalDocs: OptArg[ExternalDocumentation] = OptArg.Empty
)
object OpenApi extends HasGenCodec[OpenApi] {
  final val Version = "3.0.1"
}

/**
  * Representation of
  * [[https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.1.md#infoObject Info Object]]
  */
case class Info(
  title: String,
  version: String,
  @td license: OptArg[License] = OptArg.Empty,
  @td description: OptArg[String] = OptArg.Empty,
  @td termsOfService: OptArg[String] = OptArg.Empty,
  @td contact: OptArg[Contact] = OptArg.Empty
)
object Info extends HasGenCodec[Info]

/**
  * Representation of
  * [[https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.1.md#contactObject Contact Object]]
  */
case class Contact(
  @td name: OptArg[String] = OptArg.Empty,
  @td url: OptArg[String] = OptArg.Empty,
  @td email: OptArg[String] = OptArg.Empty
)
object Contact extends HasGenCodec[Contact]

/**
  * Representation of
  * [[https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.1.md#licenseObject License Object]]
  */
case class License(
  name: String,
  @td url: OptArg[String] = OptArg.Empty
)
object License extends HasGenCodec[License]

/**
  * Representation of
  * [[https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.1.md#serverObject Server Object]]
  */
case class Server(
  url: String,
  @td description: OptArg[String] = OptArg.Empty,
  @td serverVariables: Map[String, ServerVariable] = Map.empty
)
object Server extends HasGenCodec[Server]

/**
  * Representation of
  * [[https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.1.md#serverVariableObject Server Variable Object]]
  */
case class ServerVariable(
  default: String,
  @td enum: List[String] = Nil,
  @td description: OptArg[String] = OptArg.Empty
)
object ServerVariable extends HasGenCodec[ServerVariable]

/**
  * Representation of
  * [[https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.1.md#pathsObject Paths Object]]
  */
@transparent case class Paths(paths: Map[String, RefOr[PathItem]])
object Paths extends HasGenCodec[Paths]

/**
  * Representation of
  * [[https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.1.md#pathItemObject Path Item Object]]
  */
case class PathItem(
  @td summary: OptArg[String] = OptArg.Empty,
  @td description: OptArg[String] = OptArg.Empty,
  @td get: OptArg[Operation] = OptArg.Empty,
  @td put: OptArg[Operation] = OptArg.Empty,
  @td post: OptArg[Operation] = OptArg.Empty,
  @td delete: OptArg[Operation] = OptArg.Empty,
  @td options: OptArg[Operation] = OptArg.Empty,
  @td head: OptArg[Operation] = OptArg.Empty,
  @td patch: OptArg[Operation] = OptArg.Empty,
  @td trace: OptArg[Operation] = OptArg.Empty,
  @td servers: List[Server] = Nil,
  @td parameters: List[RefOr[Parameter]] = Nil
)
object PathItem extends HasGenCodec[PathItem]

/**
  * Representation of
  * [[https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.1.md#operationObject Operation Object]]
  */
case class Operation(
  responses: Responses,
  @td tags: List[String] = Nil,
  @td summary: OptArg[String] = OptArg.Empty,
  @td description: OptArg[String] = OptArg.Empty,
  @td externalDocs: OptArg[ExternalDocumentation] = OptArg.Empty,
  @td operationId: OptArg[String] = OptArg.Empty,
  @td parameters: List[RefOr[Parameter]] = Nil,
  @td requestBody: OptArg[RefOr[RequestBody]] = OptArg.Empty,
  @td callbacks: Map[String, RefOr[Callback]] = Map.empty,
  @td deprecated: Boolean = false,
  @td security: List[SecurityRequirement] = Nil,
  @td servers: List[Server] = Nil
)
object Operation extends HasGenCodec[Operation]

/**
  * Representation of
  * [[https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.1.md#responsesObject Responses Object]]
  */
case class Responses(
  byStatusCode: Map[Int, RefOr[Response]] = Map.empty,
  default: OptArg[RefOr[Response]] = OptArg.Empty
)
object Responses {
  final val DefaultField = "default"

  implicit val codec: GenCodec[Responses] = GenCodec.createNullableObject(
    oi => {
      var default = OptArg.empty[RefOr[Response]]
      val byStatusCode = Map.newBuilder[Int, RefOr[Response]]
      while (oi.hasNext) {
        val fi = oi.nextField()
        fi.fieldName match {
          case DefaultField =>
            default = GenCodec.read[RefOr[Response]](fi)
          case status =>
            byStatusCode += ((status.toInt, GenCodec.read[RefOr[Response]](fi)))
        }
      }
      Responses(byStatusCode.result(), default)
    },
    (oo, v) => {
      v.default.foreach(resp => GenCodec.write[RefOr[Response]](oo.writeField(DefaultField), resp))
      v.byStatusCode.foreach {
        case (status, resp) =>
          GenCodec.write[RefOr[Response]](oo.writeField(status.toString), resp)
      }
    }
  )
}

/**
  * Representation of
  * [[https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.1.md#componentsObject Components Object]]
  */
case class Components(
  @td schemas: Map[String, RefOr[Schema]] = Map.empty,
  @td responses: Map[String, RefOr[Response]] = Map.empty,
  @td parameters: Map[String, RefOr[Parameter]] = Map.empty,
  @td examples: Map[String, RefOr[Example]] = Map.empty,
  @td requestBodies: Map[String, RefOr[RequestBody]] = Map.empty,
  @td headers: Map[String, RefOr[Header]] = Map.empty,
  @td securitySchemes: Map[String, RefOr[SecurityScheme]] = Map.empty,
  @td links: Map[String, RefOr[Link]] = Map.empty,
  @td callbacks: Map[String, RefOr[Callback]] = Map.empty
)
object Components extends HasGenCodec[Components]

/**
  * Representation of
  * [[https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.1.md#securityRequirementObject Security Requirement Object]]
  */
@transparent case class SecurityRequirement(schemes: Map[String, List[String]])
object SecurityRequirement extends HasGenCodec[SecurityRequirement]

/**
  * Representation of
  * [[https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.1.md#tagObject Tag Object]]
  */
case class Tag(
  name: String,
  @td description: OptArg[String] = OptArg.Empty,
  @td externalDocs: OptArg[ExternalDocumentation] = OptArg.Empty
)
object Tag extends HasGenCodec[Tag]

/**
  * Representation of
  * [[https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.1.md#externalDocumentationObject External Documentation Object]]
  */
case class ExternalDocumentation(
  url: String,
  @td description: OptArg[String] = OptArg.Empty
)
object ExternalDocumentation extends HasGenCodec[ExternalDocumentation]

/**
  * Representation of
  * [[https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.1.md#schemaObject Schema Object]]
  */
case class Schema(
  @td `type`: OptArg[DataType] = OptArg.Empty,
  @td format: OptArg[String] = OptArg.Empty,
  @td title: OptArg[String] = OptArg.Empty,
  @td description: OptArg[String] = OptArg.Empty,
  @td nullable: Boolean = false,
  @td readOnly: Boolean = false,
  @td writeOnly: Boolean = false,
  @td xml: OptArg[Xml] = OptArg.Empty,
  @td externalDocs: OptArg[ExternalDocumentation] = OptArg.Empty,
  @td deprecated: Boolean = false,

  @td multipleOf: OptArg[BigDecimal] = OptArg.Empty,
  @td maximum: OptArg[BigDecimal] = OptArg.Empty,
  @td exclusiveMaximum: Boolean = false,
  @td minimum: OptArg[BigDecimal] = OptArg.Empty,
  @td exclusiveMinimum: Boolean = false,

  @td maxLength: OptArg[Int] = OptArg.Empty,
  @td minLength: OptArg[Int] = OptArg.Empty,
  @td pattern: OptArg[String] = OptArg.Empty,

  @td items: OptArg[RefOr[Schema]] = OptArg.Empty,
  @td maxItems: OptArg[Int] = OptArg.Empty,
  @td minItems: OptArg[Int] = OptArg.Empty,
  @td uniqueItems: Boolean = false,

  @td properties: Map[String, RefOr[Schema]] = Map.empty,
  @td additionalProperties: OptArg[RefOr[Schema]] = OptArg.Empty, //TODO: boolean value support
  @td maxProperties: OptArg[Int] = OptArg.Empty,
  @td minProperties: OptArg[Int] = OptArg.Empty,
  @td required: List[String] = Nil,

  @td allOf: List[RefOr[Schema]] = Nil,
  @td oneOf: List[RefOr[Schema]] = Nil,
  @td anyOf: List[RefOr[Schema]] = Nil,
  @td not: OptArg[RefOr[Schema]] = OptArg.Empty,
  @td discriminator: OptArg[Discriminator] = OptArg.Empty,

  @td enum: List[JsonValue] = Nil,
  @td default: OptArg[JsonValue] = OptArg.Empty
) {
  def unwrapSingleRefAllOf: RefOr[Schema] = allOf match {
    case List(ref: RefOr.Ref) if this == Schema(allOf = List(ref)) => ref
    case _ => RefOr(this)
  }
}
object Schema extends HasGenCodec[Schema] {
  final val Boolean = Schema(`type` = DataType.Boolean)
  final val Char = Schema(`type` = DataType.String, minLength = 1, maxLength = 1)
  final val Byte = Schema(`type` = DataType.Integer, format = Format.Int32,
    minimum = BigDecimal(scala.Byte.MinValue), maximum = BigDecimal(scala.Byte.MaxValue))
  final val Short = Schema(`type` = DataType.Integer, format = Format.Int32,
    minimum = BigDecimal(scala.Short.MinValue), maximum = BigDecimal(scala.Short.MaxValue))
  final val Int = Schema(`type` = DataType.Integer, format = Format.Int32)
  final val Long = Schema(`type` = DataType.Integer, format = Format.Int64)
  final val Float = Schema(`type` = DataType.Number, format = Format.Float)
  final val Double = Schema(`type` = DataType.Number, format = Format.Double)
  final val Integer = Schema(`type` = DataType.Integer)
  final val Number = Schema(`type` = DataType.Number)
  final val String = Schema(`type` = DataType.String)
  final val Date = Schema(`type` = DataType.String, format = Format.Date)
  final val DateTime = Schema(`type` = DataType.String, format = Format.DateTime)
  final val Uuid = Schema(`type` = DataType.String, format = Format.Uuid)
  final val Password = Schema(`type` = DataType.String, format = Format.Password)
  final val Binary = Schema(`type` = DataType.String, format = Format.Binary)
  final val Email = Schema(`type` = DataType.String, format = Format.Email)

  def arrayOf(items: RefOr[Schema], uniqueItems: Boolean = false): Schema =
    Schema(`type` = DataType.Array, items = items, uniqueItems = uniqueItems)

  def mapOf(properties: RefOr[Schema]): Schema =
    Schema(`type` = DataType.Object, additionalProperties = properties)

  def enumOf(values: List[String]): Schema =
    Schema(`type` = DataType.String, enum = values.map(s => JsonValue(JsonStringOutput.write(s))))

  def nullable(schema: RefOr[Schema]): Schema =
    schema.rewrapRefToAllOf.copy(nullable = true)

  implicit class RefOrOps(private val refOrSchema: RefOr[Schema]) extends AnyVal {
    /**
      * Transforms a potential schema reference into an actual [[Schema]] by wrapping the reference into
      * `allOf` property of the new schema, e.g. `{"$$ref": "#/components/schemas/Entity"}` becomes
      * `{"allOf": [{"$$ref": "#/components/schemas/Entity"}]}`.
      */
    def rewrapRefToAllOf: Schema = refOrSchema match {
      case RefOr.Value(schema) => schema
      case ref => Schema(allOf = List(ref))
    }

    def withDefaultValue(dv: Opt[Try[JsonValue]]): RefOr[Schema] =
      dv.collect({ case Success(v) => v }).fold(refOrSchema)(v => RefOr(rewrapRefToAllOf.copy(default = v)))
  }
}

object Format {
  final val Int32 = "int32"
  final val Int64 = "int64"
  final val Float = "float"
  final val Double = "double"
  final val Byte = "byte"
  final val Binary = "binary"
  final val Date = "date"
  final val DateTime = "date-time"
  final val Password = "password"
  final val Email = "email"
  final val Uuid = "uuid"
}

final class DataType(implicit enumCtx: EnumCtx) extends AbstractValueEnum {
  override val name: String = enumCtx.valName.uncapitalize
}
object DataType extends AbstractValueEnumCompanion[DataType] {
  final val String, Number, Integer, Boolean, Array, Object: Value = new DataType
}

/**
  * Representation of
  * [[https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.1.md#discriminatorObject Discriminator Object]]
  */
case class Discriminator(
  propertyName: String,
  @td mapping: Map[String, String] = Map.empty
)
object Discriminator extends HasGenCodec[Discriminator]

/**
  * Representation of
  * [[https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.1.md#xmlObject Xml Object]]
  */
case class Xml(
  @td name: OptArg[String] = OptArg.Empty,
  @td namespace: OptArg[String] = OptArg.Empty,
  @td prefix: OptArg[String] = OptArg.Empty,
  @td attribute: Boolean = false,
  @td wrapped: Boolean = false
)
object Xml extends HasGenCodec[Xml]

/**
  * Representation of
  * [[https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.1.md#responseObject Response Object]]
  */
case class Response(
  @td description: OptArg[String] = OptArg.Empty,
  @td headers: Map[String, RefOr[Header]] = Map.empty,
  @td content: Map[String, MediaType] = Map.empty,
  @td links: Map[String, RefOr[Link]] = Map.empty
)
object Response extends HasGenCodec[Response]

/**
  * Representation of
  * [[https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.1.md#parameterObject Parameter Object]]
  */
case class Parameter(
  name: String,
  in: Location,
  @td description: OptArg[String] = OptArg.Empty,
  @td required: Boolean = false,
  @td deprecated: Boolean = false,
  @td allowEmptyValue: Boolean = false,
  @td style: OptArg[Style] = OptArg.Empty,
  @td explode: OptArg[Boolean] = OptArg.Empty,
  @td allowReserved: Boolean = false,
  @td schema: OptArg[RefOr[Schema]] = OptArg.Empty,
  @td example: OptArg[JsonValue] = OptArg.Empty,
  @td examples: Map[String, RefOr[Example]] = Map.empty,
  @td content: OptArg[Entry[String, MediaType]] = OptArg.Empty
)
object Parameter extends HasGenCodec[Parameter]

case class Entry[K, V](key: K, value: V)
object Entry {
  implicit def codec[K: GenKeyCodec, V: GenCodec]: GenCodec[Entry[K, V]] =
    GenCodec.createNullableObject(
      oi => {
        val fi = oi.nextField()
        Entry(GenKeyCodec.read[K](fi.fieldName), GenCodec.read[V](fi))
      },
      (oo, entry) =>
        GenCodec.write[V](oo.writeField(GenKeyCodec.write[K](entry.key)), entry.value)
    )
}

final class Location(implicit enumCtx: EnumCtx) extends AbstractValueEnum {
  override val name: String = enumCtx.valName.uncapitalize
}
object Location extends AbstractValueEnumCompanion[Location] {
  final val Query, Header, Path, Cookie: Value = new Location
}

/**
  * Representation of
  * [[https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.1.md#style-values parameter style]]
  */
final class Style(implicit enumCtx: EnumCtx) extends AbstractValueEnum {
  override val name: String = enumCtx.valName.uncapitalize
}
object Style extends AbstractValueEnumCompanion[Style] {
  final val Matrix, Label, Form, Simple, SpaceDelimited, PipeDelimited, DeepObject: Value = new Style
}

/**
  * Representation of
  * [[https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.1.md#mediaTypeObject Media Type Object]]
  */
case class MediaType(
  @td schema: OptArg[RefOr[Schema]] = OptArg.Empty,
  @td example: OptArg[String] = OptArg.Empty, //TODO other values than strings
  @td examples: Map[String, RefOr[Example]] = Map.empty,
  @td encoding: Map[String, Encoding] = Map.empty
)
object MediaType extends HasGenCodec[MediaType]

/**
  * Representation of
  * [[https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.1.md#encodingObject Encoding Object]]
  */
case class Encoding(
  @td contentType: OptArg[String] = OptArg.Empty,
  @td headers: Map[String, RefOr[Header]] = Map.empty,
  @td style: OptArg[Style] = OptArg.Empty,
  @td explode: OptArg[Boolean] = OptArg.Empty,
  @td allowReserved: Boolean = false
)
object Encoding extends HasGenCodec[Encoding]

case class Example(
  @td summary: OptArg[String] = OptArg.Empty,
  @td description: OptArg[String] = OptArg.Empty,
  @td value: OptArg[JsonValue] = OptArg.Empty,
  @td externalValue: OptArg[String] = OptArg.Empty
)
object Example extends HasGenCodec[Example]

/**
  * Representation of
  * [[https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.1.md#requestBodyObject Request Body Object]]
  */
case class RequestBody(
  content: Map[String, MediaType],
  @td description: OptArg[String] = OptArg.Empty,
  @td required: Boolean = false
)
object RequestBody extends HasGenCodec[RequestBody]

/**
  * Representation of
  * [[https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.1.md#headerObject Header Object]]
  */
case class Header(
  @td description: OptArg[String] = OptArg.Empty,
  @td required: Boolean = false,
  @td deprecated: Boolean = false,
  @td allowEmptyValue: Boolean = false,
  @td style: OptArg[Style] = OptArg.Empty,
  @td explode: OptArg[Boolean] = OptArg.Empty,
  @td allowReserved: Boolean = false,
  @td schema: OptArg[RefOr[Schema]] = OptArg.Empty,
  @td example: OptArg[JsonValue] = OptArg.Empty,
  @td examples: Map[String, RefOr[Example]] = Map.empty,
  @td content: OptArg[Entry[String, MediaType]] = OptArg.Empty
)
object Header extends HasGenCodec[Header]

/**
  * Representation of
  * [[https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.1.md#securitySchemeObject Security Scheme Object]]
  */
@flatten("type") sealed trait SecurityScheme {
  def description: OptArg[String]
}
object SecurityScheme {
  @name("apiKey") case class ApiKey(
    name: String,
    in: Location,
    @td description: OptArg[String] = OptArg.Empty
  ) extends SecurityScheme

  @name("http") case class Http(
    scheme: String,
    @td bearerFormat: OptArg[String] = OptArg.Empty,
    @td description: OptArg[String] = OptArg.Empty
  ) extends SecurityScheme

  @name("oauth2") case class OAuth2(
    flows: OAuthFlows,
    @td description: OptArg[String] = OptArg.Empty
  ) extends SecurityScheme

  @name("openIdConnect") case class OpenIdConnect(
    openIdConnectUrl: String,
    @td description: OptArg[String] = OptArg.Empty
  ) extends SecurityScheme

  implicit val codec: GenCodec[SecurityScheme] = GenCodec.materialize
}

/**
  * Representation of
  * [[https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.1.md#oauthFlowsObject OAuth Flows Object]]
  */
case class OAuthFlows(
  @td `implicit`: OptArg[OAuthFlow] = OptArg.Empty,
  @td password: OptArg[OAuthFlow] = OptArg.Empty,
  @td clientCredentials: OptArg[OAuthFlow] = OptArg.Empty,
  @td authorizationCode: OptArg[OAuthFlow] = OptArg.Empty
)
object OAuthFlows extends HasGenCodec[OAuthFlows]

/**
  * Representation of
  * [[https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.1.md#oauthFlowObject OAuth Flow Object]]
  */
case class OAuthFlow(
  scopes: Map[String, String],
  @td authorizationUrl: OptArg[String] = OptArg.Empty,
  @td tokenUrl: OptArg[String] = OptArg.Empty,
  @td refreshUrl: OptArg[String] = OptArg.Empty
)
object OAuthFlow extends HasGenCodec[OAuthFlow]

/**
  * Representation of
  * [[https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.1.md#linkObject Link Object]]
  */
case class Link(
  @td operationRef: OptArg[String] = OptArg.Empty,
  @td operationId: OptArg[String] = OptArg.Empty,
  @td parameters: Map[String, JsonValue] = Map.empty,
  @td requestBody: OptArg[JsonValue] = OptArg.Empty,
  @td description: OptArg[String] = OptArg.Empty,
  @td server: OptArg[Server] = OptArg.Empty
)
object Link extends HasGenCodec[Link]

/**
  * Representation of
  * [[https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.1.md#callbackObject Callback Object]]
  */
@transparent case class Callback(byExpression: Map[String, PathItem])
object Callback extends HasGenCodec[Callback]

sealed trait RefOr[+A]
object RefOr {
  case class Ref(ref: String) extends RefOr[Nothing]
  case class Value[+A](value: A) extends RefOr[A]

  final val RefField = "$ref"

  def apply[A](value: A): RefOr[A] = Value(value)
  def ref[A](ref: String): RefOr[A] = Ref(ref)

  implicit def codec[A: GenCodec]: GenCodec[RefOr[A]] =
    GenCodec.createNullableObject(
      oi => {
        val poi = new PeekingObjectInput(oi)
        val refFieldInput = poi.peekField(RefField).orElse {
          if (poi.peekNextFieldName.contains(RefField)) poi.nextField().opt
          else Opt.Empty
        }
        val res = refFieldInput.map(fi => Ref(fi.readString()))
          .getOrElse(Value(GenCodec.read[A](new ObjectInputAsInput(poi))))
        poi.skipRemaining()
        res
      },
      (oo, value) => value match {
        case Ref(refstr) => oo.writeField(RefField).writeString(refstr)
        case Value(v) => GenCodec.write[A](new ObjectOutputAsOutput(oo, forwardFinish = false), v)
      }
    )
}
