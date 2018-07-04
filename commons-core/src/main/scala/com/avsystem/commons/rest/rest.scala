package com.avsystem.commons
package rest

import com.avsystem.commons.annotation.AnnotationAggregate
import com.avsystem.commons.misc.{AbstractValueEnum, AbstractValueEnumCompanion, EnumCtx}
import com.avsystem.commons.rpc._
import com.avsystem.commons.serialization.GenCodec.ReadFailure
import com.avsystem.commons.serialization.json.{JsonReader, JsonStringInput, JsonStringOutput}
import com.avsystem.commons.serialization.{GenCodec, GenKeyCodec}

import scala.collection.immutable.ListMap

sealed trait RestMethodTag extends RpcTag {
  def path: OptArg[String]
}
sealed abstract class HttpMethodTag(val method: HttpMethod) extends RestMethodTag with AnnotationAggregate
sealed abstract class BodyMethodTag(method: HttpMethod) extends HttpMethodTag(method)

final class GET(val path: OptArg[String] = OptArg.Empty) extends HttpMethodTag(HttpMethod.GET) {
  @rpcNamePrefix("GET_") type Implied
}
final class POST(val path: OptArg[String] = OptArg.Empty) extends BodyMethodTag(HttpMethod.POST) {
  @rpcNamePrefix("POST_") type Implied
}
final class PATCH(val path: OptArg[String] = OptArg.Empty) extends BodyMethodTag(HttpMethod.PATCH) {
  @rpcNamePrefix("PATCH_") type Implied
}
final class PUT(val path: OptArg[String] = OptArg.Empty) extends BodyMethodTag(HttpMethod.PUT) {
  @rpcNamePrefix("PUT_") type Implied
}
final class DELETE(val path: OptArg[String] = OptArg.Empty) extends BodyMethodTag(HttpMethod.DELETE) {
  @rpcNamePrefix("DELETE_") type Implied
}
final class Prefix(val path: OptArg[String] = OptArg.Empty) extends RestMethodTag

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
  def jsonValue: JsonValue = mimeType match {
    case HttpBody.JsonType => JsonValue(value)
    case _ => throw new ReadFailure(s"Expected application/json type, got $mimeType")
  }
}
object HttpBody {
  final val PlainType = "text/plain"
  final val JsonType = "application/json"

  def plain(value: String): HttpBody = HttpBody(value, PlainType)
  def json(json: JsonValue): HttpBody = HttpBody(json.value, JsonType)

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
      HttpBody.json(JsonValue(sb.toString))
    }

  def parseJsonBody(body: HttpBody): ListMap[String, JsonValue] =
    if (body.value.isEmpty) ListMap.empty else {
      val oi = new JsonStringInput(new JsonReader(body.jsonValue.value)).readObject()
      val builder = ListMap.newBuilder[String, JsonValue]
      while (oi.hasNext) {
        val fi = oi.nextField()
        builder += ((fi.fieldName, JsonValue(fi.readRawJson())))
      }
      builder.result()
    }

  implicit val emptyBodyForUnit: AsRawReal[HttpBody, Unit] =
    AsRawReal.create(_ => HttpBody.Empty, _ => ())
  implicit def httpBodyJsonAsRaw[T](implicit jsonAsRaw: AsRaw[JsonValue, T]): AsRaw[HttpBody, T] =
    AsRaw.create(v => HttpBody.json(jsonAsRaw.asRaw(v)))
  implicit def httpBodyJsonAsReal[T](implicit jsonAsReal: AsReal[JsonValue, T]): AsReal[HttpBody, T] =
    AsReal.create(v => jsonAsReal.asReal(v.jsonValue))
}

final class HttpMethod(implicit enumCtx: EnumCtx) extends AbstractValueEnum
object HttpMethod extends AbstractValueEnumCompanion[HttpMethod] {
  final val GET, PUT, POST, PATCH, DELETE: Value = new HttpMethod
}

case class RestHeaders(
  @multi @tagged[Path] path: List[PathValue],
  @multi @tagged[Header] headers: ListMap[String, HeaderValue],
  @multi @tagged[Query] query: ListMap[String, QueryValue]
) {
  def append(methodPath: List[PathValue], otherHeaders: RestHeaders): RestHeaders = RestHeaders(
    path ::: methodPath ::: otherHeaders.path,
    headers ++ otherHeaders.headers,
    query ++ otherHeaders.query
  )
}
object RestHeaders {
  final val Empty = RestHeaders(Nil, ListMap.empty, ListMap.empty)
}

class HttpErrorException(code: Int, payload: String)
  extends RuntimeException(s"$code: $payload")

case class RestRequest(method: HttpMethod, headers: RestHeaders, body: HttpBody)
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
@paramTag[RestParamTag, RestParamTag]
trait RawRest {
  @multi
  @tagged[Prefix]
  @paramTag[RestParamTag, Path]
  def prefix(@methodName name: String, @composite headers: RestHeaders): RawRest

  @multi
  @tagged[GET]
  @paramTag[RestParamTag, Query]
  def get(@methodName name: String, @composite headers: RestHeaders): Future[RestResponse]

  @multi
  @tagged[BodyMethodTag]
  @paramTag[RestParamTag, JsonBodyParam]
  def handle(@methodName name: String, @composite headers: RestHeaders,
    @multi @tagged[JsonBodyParam] body: IListMap[String, JsonValue]): Future[RestResponse]

  @multi
  @tagged[BodyMethodTag]
  def handleSingle(@methodName name: String, @composite headers: RestHeaders,
    @encoded @tagged[Body] body: HttpBody): Future[RestResponse]

  def asHandleRequest(metadata: RestMetadata[_]): RestRequest => Future[RestResponse] = {
    case RestRequest(method, headers, body) => metadata.resolvePath(method, headers.path).toList match {
      case List(ResolvedPath(prefixes, RpcWithPath(finalRpcName, finalPathParams), singleBody)) =>
        val finalRawRest = prefixes.foldLeft(this) {
          case (rawRest, RpcWithPath(rpcName, pathParams)) =>
            rawRest.prefix(rpcName, headers.copy(path = pathParams))
        }
        val finalHeaders = headers.copy(path = finalPathParams)

        if (method == HttpMethod.GET) finalRawRest.get(finalRpcName, finalHeaders)
        else if (singleBody) finalRawRest.handleSingle(finalRpcName, finalHeaders, body)
        else finalRawRest.handle(finalRpcName, finalHeaders, HttpBody.parseJsonBody(body))

      case Nil =>
        val pathStr = headers.path.iterator.map(_.value).mkString("/")
        Future.successful(RestResponse(404, HttpBody.plain(s"path $pathStr not found")))

      case multiple =>
        val pathStr = headers.path.iterator.map(_.value).mkString("/")
        val callsRepr = multiple.iterator.map(p => s"  ${p.rpcChainRepr}").mkString("\n", "\n", "")
        throw new IllegalArgumentException(s"path $pathStr is ambiguous, it could map to following calls:$callsRepr")
    }
  }
}

object RawRest extends RawRpcCompanion[RawRest] {
  def fromHandleRequest[Real: AsRealRpc : RestMetadata](handleRequest: RestRequest => Future[RestResponse]): Real =
    RawRest.asReal(new DefaultRawRest(RestMetadata[Real], RestHeaders.Empty, handleRequest))

  def asHandleRequest[Real: AsRawRpc : RestMetadata](real: Real): RestRequest => Future[RestResponse] =
    RawRest.asRaw(real).asHandleRequest(RestMetadata[Real])

  private final class DefaultRawRest(
    metadata: RestMetadata[_],
    prefixHeaders: RestHeaders,
    handleRequest: RestRequest => Future[RestResponse]
  ) extends RawRest {

    def prefix(name: String, headers: RestHeaders): RawRest = {
      val prefixMeta = metadata.prefixMethods.getOrElse(name,
        throw new IllegalArgumentException(s"no such prefix method: $name"))
      val newHeaders = prefixHeaders.append(prefixMeta.methodPath, headers)
      new DefaultRawRest(prefixMeta.result.value, newHeaders, handleRequest)
    }

    def get(name: String, headers: RestHeaders): Future[RestResponse] =
      handleSingle(name, headers, HttpBody.Empty)

    def handle(name: String, headers: RestHeaders, body: IListMap[String, JsonValue]): Future[RestResponse] = {
      val methodMeta = metadata.httpMethods.getOrElse(name,
        throw new IllegalArgumentException(s"no such HTTP method: $name"))
      val newHeaders = prefixHeaders.append(methodMeta.methodPath, headers)
      handleRequest(RestRequest(methodMeta.method, newHeaders, HttpBody.createJsonBody(body)))
    }

    def handleSingle(name: String, headers: RestHeaders, body: HttpBody): Future[RestResponse] = {
      val methodMeta = metadata.httpMethods.getOrElse(name,
        throw new IllegalArgumentException(s"no such HTTP method: $name"))
      val newHeaders = prefixHeaders.append(methodMeta.methodPath, headers)
      handleRequest(RestRequest(methodMeta.method, newHeaders, body))
    }
  }

  trait ClientMacroInstances[Real] {
    def metadata: RestMetadata[Real]
    def asReal: AsRealRpc[Real]
  }

  trait ServerMacroInstances[Real] {
    def metadata: RestMetadata[Real]
    def asRaw: AsRawRpc[Real]
  }

  trait FullMacroInstances[Real] extends ClientMacroInstances[Real] with ServerMacroInstances[Real]

  implicit def clientInstances[Real]: ClientMacroInstances[Real] = macro macros.rest.RestMacros.instances[Real]
  implicit def serverInstances[Real]: ServerMacroInstances[Real] = macro macros.rest.RestMacros.instances[Real]
  implicit def fullInstances[Real]: FullMacroInstances[Real] = macro macros.rest.RestMacros.instances[Real]
}

abstract class RestClientApiCompanion[Real](implicit instances: RawRest.ClientMacroInstances[Real]) {
  implicit def restMetadata: RestMetadata[Real] = instances.metadata
  implicit def rawRestAsReal: RawRest.AsRealRpc[Real] = instances.asReal
}

abstract class RestServerApiCompanion[Real](implicit instances: RawRest.ServerMacroInstances[Real]) {
  implicit def restMetadata: RestMetadata[Real] = instances.metadata
  implicit def realAsRawRest: RawRest.AsRawRpc[Real] = instances.asRaw
}

abstract class RestApiCompanion[Real](implicit instances: RawRest.FullMacroInstances[Real]) {
  implicit def restMetadata: RestMetadata[Real] = instances.metadata
  implicit def rawRestAsReal: RawRest.AsRealRpc[Real] = instances.asReal
  implicit def realAsRawRest: RawRest.AsRawRpc[Real] = instances.asRaw
}

case class RpcWithPath(rpcName: String, pathParams: List[PathValue])
case class ResolvedPath(prefixes: List[RpcWithPath], finalCall: RpcWithPath, singleBody: Boolean) {
  def prepend(rpcName: String, pathParams: List[PathValue]): ResolvedPath =
    copy(prefixes = RpcWithPath(rpcName, pathParams) :: prefixes)

  def rpcChainRepr: String =
    prefixes.iterator.map(_.rpcName).mkString("", "->", s"->${finalCall.rpcName}")
}

@methodTag[RestMethodTag, Prefix]
@paramTag[RestParamTag, JsonBodyParam]
case class RestMetadata[T](
  @multi @tagged[Prefix] prefixMethods: Map[String, PrefixMetadata[_]],
  @multi @tagged[HttpMethodTag] httpMethods: Map[String, HttpMethodMetadata[_]]
) {
  def resolvePath(method: HttpMethod, path: List[PathValue]): Iterator[ResolvedPath] = {
    val asFinalCall = for {
      (rpcName, m) <- httpMethods.iterator if m.method == method
      (pathParams, Nil) <- m.extractPathParams(path)
    } yield ResolvedPath(Nil, RpcWithPath(rpcName, pathParams), m.singleBody)

    val usingPrefix = for {
      (rpcName, prefix) <- prefixMethods.iterator
      (pathParams, pathTail) <- prefix.extractPathParams(path).iterator
      suffixPath <- prefix.result.value.resolvePath(method, pathTail)
    } yield suffixPath.prepend(rpcName, pathParams)

    asFinalCall ++ usingPrefix
  }
}
object RestMetadata extends RpcMetadataCompanion[RestMetadata]

sealed abstract class RestMethodMetadata[T] extends TypedMetadata[T] {
  def name: String
  def tagPath: Opt[String]
  def headersMetadata: RestHeadersMetadata

  val methodPath: List[PathValue] =
    tagPath.fold(List(PathValue(name)))(_.split("/").iterator.filter(_.nonEmpty).map(PathValue).toList)

  def extractPathParams(path: List[PathValue]): Opt[(List[PathValue], List[PathValue])] = {
    def suffix(prefix: List[PathValue], path: List[PathValue]): Opt[List[PathValue]] = (prefix, path) match {
      case (Nil, result) => Opt(result)
      case (prefixHead :: prefixTail, pathHead :: pathTail) if prefixHead == pathHead =>
        suffix(prefixTail, pathTail)
      case _ => Opt.Empty
    }
    suffix(methodPath, path).flatMap(headersMetadata.extractPathParams)
  }
}

@paramTag[RestParamTag, Path]
case class PrefixMetadata[T](
  @reifyName name: String,
  @optional @reifyAnnot methodTag: Opt[Prefix],
  @composite headersMetadata: RestHeadersMetadata,
  @checked @infer result: RestMetadata.Lazy[T]
) extends RestMethodMetadata[T] {
  def tagPath: Opt[String] = methodTag.flatMap(_.path.toOpt)
}

case class HttpMethodMetadata[T](
  @reifyName name: String,
  @reifyAnnot methodTag: HttpMethodTag,
  @composite headersMetadata: RestHeadersMetadata,
  @multi @tagged[BodyTag] bodyParams: Map[String, ParamMetadata[_]]
) extends RestMethodMetadata[Future[T]] {
  val method: HttpMethod = methodTag.method
  val singleBody: Boolean = bodyParams.values.exists(_.singleBody)
  def tagPath: Opt[String] = methodTag.path.toOpt
}

case class RestHeadersMetadata(
  @multi @tagged[Path] path: List[ParamMetadata[_]],
  @multi @tagged[Header] headers: Map[String, ParamMetadata[_]],
  @multi @tagged[Query] query: Map[String, ParamMetadata[_]]
) {
  val pathLength: Int = path.size

  def extractPathParams(path: List[PathValue]): Opt[(List[PathValue], List[PathValue])] = {
    def loop(acc: List[PathValue], path: List[PathValue], index: Int): Opt[(List[PathValue], List[PathValue])] =
      if (index == 0) Opt((acc.reverse, path))
      else path match {
        case head :: tail => loop(head :: acc, tail, index - 1)
        case Nil => Opt.Empty
      }
    loop(Nil, path, pathLength)
  }
}

case class ParamMetadata[T](@hasAnnot[Body] singleBody: Boolean)
  extends TypedMetadata[T]
