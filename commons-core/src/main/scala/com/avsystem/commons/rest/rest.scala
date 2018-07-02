package com.avsystem.commons
package rest

import com.avsystem.commons.annotation.AnnotationAggregate
import com.avsystem.commons.misc.{AbstractValueEnum, AbstractValueEnumCompanion, EnumCtx}
import com.avsystem.commons.rpc._
import com.avsystem.commons.serialization.json.JsonStringOutput

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
final class Header extends RestParamTag
final class Path extends RestParamTag
final class Query extends RestParamTag
final class BodyParam extends RestParamTag
final class Body extends RestParamTag

sealed trait RestValue extends Any {
  def value: String
}
case class PathValue(value: String) extends AnyVal with RestValue
case class HeaderValue(value: String) extends AnyVal with RestValue
case class QueryParamValue(value: String) extends AnyVal with RestValue
case class BodyValue(value: String) extends AnyVal with RestValue
object BodyValue {
  def combineIntoObject(fields: BIterable[(String, BodyValue)]): BodyValue = {
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
object RestValue {
  // AsReal, AsRaw, itp
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
  @multi @tagged[Query] query: ListMap[String, PathValue]
) {
  def append(pathName: PathValue, otherHeaders: RestHeaders): RestHeaders = RestHeaders(
    path ++ (pathName :: otherHeaders.path),
    headers ++ otherHeaders.headers,
    query ++ otherHeaders.query
  )
}
object RestHeaders {
  final val Empty = RestHeaders(Nil, ListMap.empty, ListMap.empty)
}

case class RestRequest(method: HttpRestMethod, headers: RestHeaders, body: BodyValue)
case class RestResponse(code: Int, body: BodyValue)

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
