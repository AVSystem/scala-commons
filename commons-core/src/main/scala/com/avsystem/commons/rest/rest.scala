package com.avsystem.commons
package rest

import com.avsystem.commons.annotation.AnnotationAggregate
import com.avsystem.commons.misc.{AbstractValueEnum, AbstractValueEnumCompanion, EnumCtx}
import com.avsystem.commons.rpc._
import com.avsystem.commons.serialization.GenCodec.ReadFailure
import com.avsystem.commons.serialization.json.{JsonReader, JsonStringInput, JsonStringOutput}
import com.avsystem.commons.serialization.{GenCodec, GenKeyCodec}

import scala.collection.immutable.ListMap

sealed trait RestMethodTag extends RpcTag
sealed trait HttpMethodTag extends RestMethodTag with AnnotationAggregate
final class GET extends HttpMethodTag {
  @rpcNamePrefix("GET_") type Implied
}
final class POST extends HttpMethodTag {
  @rpcNamePrefix("POST_") type Implied
}
final class PATCH extends HttpMethodTag {
  @rpcNamePrefix("PATCH_") type Implied
}
final class PUT extends HttpMethodTag {
  @rpcNamePrefix("PUT_") type Implied
}
final class DELETE extends HttpMethodTag {
  @rpcNamePrefix("DELETE_") type Implied
}
sealed trait Prefix extends RestMethodTag

sealed trait RestParamTag extends RpcTag
final class Path extends RestParamTag
final class Header extends RestParamTag
final class Query extends RestParamTag
sealed trait BodyTag extends RestParamTag
final class JsonBodyParam extends BodyTag
final class Body extends BodyTag

sealed trait RestValue extends Any {
  def value: String
}
case class PathValue(value: String) extends AnyVal with RestValue
case class HeaderValue(value: String) extends AnyVal with RestValue
case class QueryValue(value: String) extends AnyVal with RestValue
case class JsonValue(value: String) extends AnyVal with RestValue
object RestValue {
  implicit def pathValueDefaultAsRealRaw[T: GenKeyCodec]: AsRawReal[PathValue, T] =
    AsRawReal.create(v => PathValue(GenKeyCodec.write[T](v)), v => GenKeyCodec.read[T](v.value))
  implicit def headerValueDefaultAsRealRaw[T: GenKeyCodec]: AsRawReal[HeaderValue, T] =
    AsRawReal.create(v => HeaderValue(GenKeyCodec.write[T](v)), v => GenKeyCodec.read[T](v.value))
  implicit def queryValueDefaultAsRealRaw[T: GenKeyCodec]: AsRawReal[QueryValue, T] =
    AsRawReal.create(v => QueryValue(GenKeyCodec.write[T](v)), v => GenKeyCodec.read[T](v.value))
  implicit def jsonValueDefaultAsRealRaw[T: GenCodec]: AsRawReal[JsonValue, T] =
    AsRawReal.create(v => JsonValue(JsonStringOutput.write[T](v)), v => JsonStringInput.read[T](v.value))
}

case class HttpBody(value: String, mimeType: String) {
  def jsonValue: String = mimeType match {
    case HttpBody.JsonType => value
    case _ => throw new ReadFailure(s"Expected application/json type, got $mimeType")
  }
}
object HttpBody {
  final val PlainType = "text/plain"
  final val JsonType = "application/json"

  def plain(value: String): HttpBody = HttpBody(value, PlainType)
  def json(value: String): HttpBody = HttpBody(value, JsonType)

  final val Empty: HttpBody = HttpBody.plain("")

  def createJsonBody(fields: BIterable[(String, JsonValue)]): HttpBody =
    if (fields.isEmpty) HttpBody.Empty else {
      val sb = new JStringBuilder
      val oo = new JsonStringOutput(sb).writeObject()
      fields.foreach {
        case (key, JsonValue(json)) =>
          oo.writeField(key).writeRawJson(json)
      }
      oo.finish()
      HttpBody.json(sb.toString)
    }

  def parseJsonBody(body: HttpBody): ListMap[String, JsonValue] =
    if (body.value.isEmpty) ListMap.empty else {
      val oi = new JsonStringInput(new JsonReader(body.jsonValue)).readObject()
      val builder = ListMap.newBuilder[String, JsonValue]
      while (oi.hasNext) {
        val fi = oi.nextField()
        builder += ((fi.fieldName, JsonValue(fi.readRawJson())))
      }
      builder.result()
    }

  implicit def httpBodyJsonAsRawReal[T: GenCodec]: AsRawReal[HttpBody, T] =
    AsRawReal.create(v => HttpBody.json(JsonStringOutput.write[T](v)), v => JsonStringInput.read[T](v.jsonValue))
}

final class HttpRestMethod(implicit enumCtx: EnumCtx) extends AbstractValueEnum {
  def toRpcName(pathName: PathValue): String =
    s"${name}_${pathName.value}"
}
object HttpRestMethod extends AbstractValueEnumCompanion[HttpRestMethod] {
  final val GET, PUT, POST, PATCH, DELETE: Value = new HttpRestMethod

  def parseRpcName(rpcName: String): (HttpRestMethod, PathValue) = rpcName.split("_", 2) match {
    case Array(httpMethod, pathName) =>
      val httpRestMethod = byName.getOrElse(httpMethod,
        throw new IllegalArgumentException(s"Unknown REST HTTP method: $httpMethod"))
      val pathValue = PathValue(pathName)
      (httpRestMethod, pathValue)
    case _ =>
      throw new IllegalArgumentException(s"Bad RPC name for REST method: $rpcName")
  }
}

case class RestHeaders(
  @multi @tagged[Path] path: List[PathValue],
  @multi @tagged[Header] headers: ListMap[String, HeaderValue],
  @multi @tagged[Query] query: ListMap[String, QueryValue]
) {
  def append(pathName: PathValue, otherHeaders: RestHeaders): RestHeaders = RestHeaders(
    path ++ (pathName :: otherHeaders.path),
    headers ++ otherHeaders.headers,
    query ++ otherHeaders.query
  )

  def extractPathName: (PathValue, RestHeaders) = path match {
    case pathName :: pathTail =>
      (pathName, copy(path = pathTail))
    case Nil =>
      throw new IllegalArgumentException("empty path")
  }

  def extractPrefix(metadata: RestHeadersMetadata): (RestHeaders, RestHeaders) = {
    val (prefixPath, tailPath) = path.splitAt(metadata.pathSize)
    (copy(path = prefixPath), copy(path = tailPath))
  }
}
object RestHeaders {
  final val Empty = RestHeaders(Nil, ListMap.empty, ListMap.empty)
}

class HttpErrorException(code: Int, payload: String)
  extends RuntimeException(s"$code: $payload")

case class RestRequest(method: HttpRestMethod, headers: RestHeaders, body: HttpBody)
case class RestResponse(code: Int, body: HttpBody)
object RestResponse {
  implicit def defaultFutureAsRaw[T](implicit bodyAsRaw: AsRaw[HttpBody, T]): AsRaw[Future[RestResponse], Future[T]] =
    AsRaw.create(_.mapNow(v => RestResponse(200, bodyAsRaw.asRaw(v))))
  implicit def defaultFutureAsReal[T](implicit bodyAsReal: AsReal[HttpBody, T]): AsReal[Future[RestResponse], Future[T]] =
    AsReal.create(_.mapNow {
      case RestResponse(200, body) => bodyAsReal.asReal(body)
      case RestResponse(code, body) => throw new HttpErrorException(code, body.value)
    })
}

@methodTag[RestMethodTag, Prefix]
@paramTag[RestParamTag, JsonBodyParam]
trait RawRest {
  @multi
  @tagged[Prefix]
  @paramTag[RestParamTag, Path]
  def prefix(@methodName name: String, @composite headers: RestHeaders): RawRest

  @multi
  @tagged[HttpMethodTag]
  def handle(@methodName name: String, @composite headers: RestHeaders,
    @multi @tagged[JsonBodyParam] body: IListMap[String, JsonValue]): Future[RestResponse]

  @multi
  @tagged[HttpMethodTag]
  def handleSingle(@methodName name: String, @composite headers: RestHeaders,
    @encoded @tagged[Body] body: HttpBody): Future[RestResponse]

  def asHandleRequest(metadata: RestMetadata[_]): RestRequest => Future[RestResponse] = request => {
    val (pathName, headers) = request.headers.extractPathName

    def forPrefix: Option[Future[RestResponse]] =
      metadata.prefixMethods.get(pathName.value).map { prefixMeta =>
        val (prefixHeaders, restOfHeaders) = headers.extractPrefix(prefixMeta.headersMetadata)
        prefix(pathName.value, prefixHeaders)
          .asHandleRequest(prefixMeta.result.value)(request.copy(headers = restOfHeaders))
      }

    def forHttpMethod: Option[Future[RestResponse]] = {
      val rpcName = request.method.toRpcName(pathName)
      metadata.httpMethods.get(rpcName).map { httpMeta =>
        if (httpMeta.singleBody) handleSingle(rpcName, headers, request.body)
        else handle(rpcName, headers, HttpBody.parseJsonBody(request.body))
      }
    }

    def notFound: Future[RestResponse] =
      Future.successful(RestResponse(404, HttpBody.plain(s"path ${pathName.value} not found")))

    forPrefix orElse forHttpMethod getOrElse notFound
  }
}

object RawRest extends RawRpcCompanion[RawRest] {
  def fromHandleRequest[Real: AsRealRpc](handleRequest: RestRequest => Future[RestResponse]): Real =
    RawRest.asReal(RawRest(handleRequest))

  def asHandleRequest[Real: AsRawRpc : RestMetadata](real: Real): RestRequest => Future[RestResponse] =
    RawRest.asRaw(real).asHandleRequest(RestMetadata[Real])

  private final class DefaultRawRest(
    prefixHeaders: RestHeaders, handleRequest: RestRequest => Future[RestResponse]) extends RawRest {

    def prefix(name: String, headers: RestHeaders): RawRest =
      new DefaultRawRest(prefixHeaders.append(PathValue(name), headers), handleRequest)

    def handle(name: String, headers: RestHeaders, body: IListMap[String, JsonValue]): Future[RestResponse] = {
      val (method, pathName) = HttpRestMethod.parseRpcName(name)
      handleRequest(RestRequest(method, prefixHeaders.append(pathName, headers), HttpBody.createJsonBody(body)))
    }

    def handleSingle(name: String, headers: RestHeaders, body: HttpBody): Future[RestResponse] = {
      val (method, pathName) = HttpRestMethod.parseRpcName(name)
      handleRequest(RestRequest(method, prefixHeaders.append(pathName, headers), body))
    }
  }

  def apply(handleRequest: RestRequest => Future[RestResponse]): RawRest =
    new DefaultRawRest(RestHeaders.Empty, handleRequest)

  trait ClientMacroInstances[Real] {
    def asReal: AsRealRpc[Real]
  }

  trait ServerMacroInstances[Real] {
    def metadata: RestMetadata[Real]
    def asRaw: AsRawRpc[Real]
  }

  trait FullMacroInstances[Real] {
    def metadata: RestMetadata[Real]
    def asRawReal: AsRawRealRpc[Real]
  }

  implicit def clientInstances[Real]: ClientMacroInstances[Real] = macro macros.rest.RestMacros.instances[Real]
  implicit def serverInstances[Real]: ServerMacroInstances[Real] = macro macros.rest.RestMacros.instances[Real]
  implicit def fullInstances[Real]: FullMacroInstances[Real] = macro macros.rest.RestMacros.instances[Real]
}

abstract class RestClientApiCompanion[Real](implicit instances: RawRest.ClientMacroInstances[Real]) {
  implicit def rawRestAsReal: RawRest.AsRealRpc[Real] = instances.asReal
}

abstract class RestServerApiCompanion[Real](implicit instances: RawRest.ServerMacroInstances[Real]) {
  implicit def restMetadata: RestMetadata[Real] = instances.metadata
  implicit def realAsRawRest: RawRest.AsRawRpc[Real] = instances.asRaw
}

abstract class RestApiCompanion[Real](implicit instances: RawRest.FullMacroInstances[Real]) {
  implicit def restMetadata: RestMetadata[Real] = instances.metadata
  implicit def restAsRealRaw: RawRest.AsRawRealRpc[Real] = instances.asRawReal
}

@methodTag[RestMethodTag, Prefix]
@paramTag[RestParamTag, JsonBodyParam]
case class RestMetadata[T](
  @multi @tagged[Prefix] prefixMethods: Map[String, PrefixMetadata[_]],
  @multi @tagged[HttpMethodTag] httpMethods: Map[String, HttpMethodMetadata[_]]
)
object RestMetadata extends RpcMetadataCompanion[RestMetadata]

@paramTag[RestParamTag, Path]
case class PrefixMetadata[T](
  @composite headersMetadata: RestHeadersMetadata,
  @checked @infer result: RestMetadata.Lazy[T]
) extends TypedMetadata[T]

case class HttpMethodMetadata[T](
  @reifyName(rpcName = true) rpcName: String,
  @composite headersParams: RestHeadersMetadata,
  @multi @tagged[BodyTag] bodyParams: Map[String, ParamMetadata[_]]
) extends TypedMetadata[Future[T]] {
  val (httpMethod, pathName) = HttpRestMethod.parseRpcName(rpcName)
  val singleBody: Boolean = bodyParams.values.exists(_.singleBody)
}

case class RestHeadersMetadata(
  @multi @tagged[Path] path: List[ParamMetadata[_]],
  @multi @tagged[Header] headers: Map[String, ParamMetadata[_]],
  @multi @tagged[Query] query: Map[String, ParamMetadata[_]]
) {
  val pathSize: Int = path.size
}

case class ParamMetadata[T](
  @hasAnnot[Body] singleBody: Boolean
) extends TypedMetadata[T]
