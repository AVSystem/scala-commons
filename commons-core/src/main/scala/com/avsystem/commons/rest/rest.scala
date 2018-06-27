package com.avsystem.commons
package rest

import com.avsystem.commons.rpc.RpcTag

import scala.collection.immutable.ListMap

sealed trait RestMethodTag extends RpcTag
final class GET extends RestMethodTag
final class POST extends RestMethodTag
final class PATCH extends RestMethodTag
final class PUT extends RestMethodTag
final class DELETE extends RestMethodTag
sealed trait Sub extends RestMethodTag

sealed trait RestParamTag extends RpcTag
final class Header extends RestParamTag
final class Path extends RestParamTag
final class QueryParam extends RestParamTag
final class BodyParam extends RestParamTag
final class Body extends RestParamTag

sealed trait RestValue extends Any {
  def value: String
}
case class PathValue(value: String) extends AnyVal with RestValue
case class HeaderValue(value: String) extends AnyVal with RestValue
case class QueryParamValue(value: String) extends AnyVal with RestValue
case class BodyValue(value: String) extends AnyVal with RestValue
object RestValue {
  // AsReal, AsRaw, itp
}

sealed abstract class RestMethod {
  def name: String
  def singleBody: Boolean
}
object RestMethod {
  case class Get(name: String, singleBody: Boolean) extends RestMethod
  case class Put(name: String, singleBody: Boolean) extends RestMethod
  case class Post(name: String, singleBody: Boolean) extends RestMethod
  case class Patch(name: String, singleBody: Boolean) extends RestMethod
  case class Delete(name: String, singleBody: Boolean) extends RestMethod
}

case class RestHeaders(
  path: List[PathValue],
  headers: List[(String, HeaderValue)],
  query: List[(String, QueryParamValue)]
) {
  def append(name: String,
    pathParams: List[PathValue],
    headerParams: ListMap[String, HeaderValue],
    queryParams: ListMap[String, QueryParamValue]
  ): RestHeaders =
    RestHeaders(path ++ (PathValue(name) :: pathParams), headers ++ headerParams, query ++ queryParams)
}
case class RestRequest(method: RestMethod, headers: RestHeaders, body: BodyValue)
case class RestResponse(code: Int, body: BodyValue)

trait AbstractRawRest extends RawRest {
  def headers: RestHeaders
  def withHeaders(headers: RestHeaders): AbstractRawRest

  def handle(request: RestRequest): Future[RestResponse]
}
