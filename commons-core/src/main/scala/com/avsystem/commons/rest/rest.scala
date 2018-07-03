package com.avsystem.commons
package rest

import com.avsystem.commons.annotation.AnnotationAggregate
import com.avsystem.commons.misc.{AbstractValueEnum, AbstractValueEnumCompanion, EnumCtx}
import com.avsystem.commons.rpc._
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
final class BodyParam extends BodyTag
final class Body extends BodyTag

sealed trait RestValue extends Any {
  def value: String
}
case class PathValue(value: String) extends AnyVal with RestValue
case class HeaderValue(value: String) extends AnyVal with RestValue
case class QueryValue(value: String) extends AnyVal with RestValue
case class BodyValue(value: String) extends AnyVal with RestValue
object BodyValue {
  def combineIntoObject(fields: BIterable[(String, BodyValue)]): BodyValue = {
    if (fields.isEmpty) {
      BodyValue("")
    } else {
      val sb = new JStringBuilder
      val oo = new JsonStringOutput(sb).writeObject()
      fields.foreach {
        case (key, BodyValue(json)) =>
          oo.writeField(key).writeRawJson(json)
      }
      oo.finish()
      BodyValue(sb.toString)
    }
  }
  def uncombineFromObject(body: BodyValue): ListMap[String, BodyValue] = {
    if (body.value.isEmpty) {
      ListMap.empty
    } else {
      val oi = new JsonStringInput(new JsonReader(body.value)).readObject()
      val builder = ListMap.newBuilder[String, BodyValue]
      while (oi.hasNext) {
        val fi = oi.nextField()
        builder += ((fi.fieldName, BodyValue(fi.readRawJson())))
      }
      builder.result()
    }
  }
}
object RestValue {
  implicit def pathValueDefaultAsRealRaw[T: GenKeyCodec]: AsRawReal[PathValue, T] =
    AsRawReal.create(v => PathValue(GenKeyCodec.write[T](v)), v => GenKeyCodec.read[T](v.value))
  implicit def headerValueDefaultAsRealRaw[T: GenKeyCodec]: AsRawReal[HeaderValue, T] =
    AsRawReal.create(v => HeaderValue(GenKeyCodec.write[T](v)), v => GenKeyCodec.read[T](v.value))
  implicit def queryValueDefaultAsRealRaw[T: GenKeyCodec]: AsRawReal[QueryValue, T] =
    AsRawReal.create(v => QueryValue(GenKeyCodec.write[T](v)), v => GenKeyCodec.read[T](v.value))
  implicit def bodyValueDefaultAsRealRaw[T: GenCodec]: AsRawReal[BodyValue, T] =
    AsRawReal.create(v => BodyValue(JsonStringOutput.write[T](v)), v => JsonStringInput.read[T](v.value))
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

case class RestRequest(method: HttpRestMethod, headers: RestHeaders, body: BodyValue)
case class RestResponse(code: Int, body: BodyValue)
object RestResponse {
  implicit def genCodecBasedFutureAsRawReal[T: GenCodec]: AsRawReal[Future[RestResponse], Future[T]] =
    AsRawReal.create(
      _.mapNow(v => RestResponse(200, BodyValue(JsonStringOutput.write[T](v)))),
      _.mapNow {
        case RestResponse(200, BodyValue(json)) => JsonStringInput.read[T](json)
        case RestResponse(code, BodyValue(payload)) => throw new HttpErrorException(code, payload)
      }
    )
}

@methodTag[RestMethodTag, Prefix]
@paramTag[RestParamTag, BodyParam]
trait RawRest {
  @multi
  @tagged[Prefix]
  @paramTag[RestParamTag, Path]
  def prefix(@methodName name: String, @composite headers: RestHeaders): RawRest

  @multi
  @tagged[HttpMethodTag]
  def handle(@methodName name: String, @composite headers: RestHeaders,
    @multi @tagged[BodyParam] body: IListMap[String, BodyValue]): Future[RestResponse]

  @multi
  @tagged[HttpMethodTag]
  def handleSingle(@methodName name: String, @composite headers: RestHeaders,
    @encoded @tagged[Body] body: BodyValue): Future[RestResponse]

  def asHandleRequest(metadata: RestMetadata[_]): RestRequest => Future[RestResponse] = request => {
    val (pathName, headers) = request.headers.extractPathName

    def forPrefix =
      metadata.prefixMethods.get(pathName.value).map { prefixMeta =>
        val (prefixHeaders, restOfHeaders) = headers.extractPrefix(prefixMeta.headersMetadata)
        prefix(pathName.value, prefixHeaders)
          .asHandleRequest(prefixMeta.result.value)(request.copy(headers = restOfHeaders))
      }

    def forHttpMethod = {
      val rpcName = request.method.toRpcName(pathName)
      metadata.httpMethods.get(rpcName).map { httpMeta =>
        if (httpMeta.singleBody) handleSingle(rpcName, headers, request.body)
        else handle(rpcName, headers, BodyValue.uncombineFromObject(request.body))
      }
    }

    def notFound =
      Future.successful(RestResponse(404, BodyValue(s"path ${pathName.value} not found")))

    forPrefix orElse forHttpMethod getOrElse notFound
  }
}

object RawRest extends RawRpcCompanion[RawRest] {
  private final class DefaultRawRest(
    prefixHeaders: RestHeaders, handleRequest: RestRequest => Future[RestResponse]) extends RawRest {

    def prefix(name: String, headers: RestHeaders): RawRest =
      new DefaultRawRest(prefixHeaders.append(PathValue(name), headers), handleRequest)

    def handle(name: String, headers: RestHeaders, body: IListMap[String, BodyValue]): Future[RestResponse] = {
      val (method, pathName) = HttpRestMethod.parseRpcName(name)
      handleRequest(RestRequest(method, prefixHeaders.append(pathName, headers), BodyValue.combineIntoObject(body)))
    }

    def handleSingle(name: String, headers: RestHeaders, body: BodyValue): Future[RestResponse] = {
      val (method, pathName) = HttpRestMethod.parseRpcName(name)
      handleRequest(RestRequest(method, prefixHeaders.append(pathName, headers), body))
    }
  }

  def apply(handleRequest: RestRequest => Future[RestResponse]): RawRest =
    new DefaultRawRest(RestHeaders.Empty, handleRequest)
}

@methodTag[RestMethodTag, Prefix]
@paramTag[RestParamTag, BodyParam]
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
