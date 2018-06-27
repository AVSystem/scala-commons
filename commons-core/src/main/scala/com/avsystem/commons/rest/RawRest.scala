package com.avsystem.commons
package rest

import com.avsystem.commons.rpc.{RawRpcCompanion, encoded, methodTag, multi, paramTag, tagged}

import scala.collection.immutable.ListMap

@methodTag[RestMethodTag, Sub]
@paramTag[RestParamTag, BodyParam]
trait RawRest {
  @multi
  @tagged[Sub]
  @paramTag[RestParamTag, Path]
  def sub(name: String)(
    @multi @tagged[Path] pathParams: List[PathValue],
    @multi @tagged[Header] headerParams: ListMap[String, HeaderValue],
    @multi @tagged[QueryParam] urlParams: ListMap[String, PathValue]
  ): RawRest

  @multi
  @tagged[GET]
  def get(name: String)(
    @multi @tagged[Path] pathParams: List[PathValue],
    @multi @tagged[Header] headerParams: ListMap[String, HeaderValue],
    @multi @tagged[QueryParam] urlParams: ListMap[String, PathValue],
    @multi @tagged[BodyParam] bodyParams: ListMap[String, BodyValue]
  ): Future[RestResponse]

  @multi
  @tagged[GET]
  def getSingle(name: String)(
    @multi @tagged[Path] pathParams: List[PathValue],
    @multi @tagged[Header] headerParams: ListMap[String, HeaderValue],
    @multi @tagged[QueryParam] urlParams: ListMap[String, PathValue],
    @encoded @tagged[Body] body: BodyValue
  ): Future[RestResponse]

  @multi
  @tagged[POST]
  def post(name: String)(
    @multi @tagged[Path] pathParams: List[PathValue],
    @multi @tagged[Header] headerParams: ListMap[String, HeaderValue],
    @multi @tagged[QueryParam] urlParams: ListMap[String, PathValue],
    @multi @tagged[BodyParam] bodyParams: ListMap[String, BodyValue]
  ): Future[RestResponse]

  @multi
  @tagged[POST]
  def postSingle(name: String)(
    @multi @tagged[Path] pathParams: List[PathValue],
    @multi @tagged[Header] headerParams: ListMap[String, HeaderValue],
    @multi @tagged[QueryParam] urlParams: ListMap[String, PathValue],
    @encoded @tagged[Body] body: BodyValue
  ): Future[RestResponse]

  @multi
  @tagged[PATCH]
  def patch(name: String)(
    @multi @tagged[Path] pathParams: List[PathValue],
    @multi @tagged[Header] headerParams: ListMap[String, HeaderValue],
    @multi @tagged[QueryParam] urlParams: ListMap[String, PathValue],
    @multi @tagged[BodyParam] bodyParams: ListMap[String, BodyValue]
  ): Future[RestResponse]

  @multi
  @tagged[PATCH]
  def patchSingle(name: String)(
    @multi @tagged[Path] pathParams: List[PathValue],
    @multi @tagged[Header] headerParams: ListMap[String, HeaderValue],
    @multi @tagged[QueryParam] urlParams: ListMap[String, PathValue],
    @encoded @tagged[Body] body: BodyValue
  ): Future[RestResponse]

  @multi
  @tagged[PUT]
  def put(name: String)(
    @multi @tagged[Path] pathParams: List[PathValue],
    @multi @tagged[Header] headerParams: ListMap[String, HeaderValue],
    @multi @tagged[QueryParam] urlParams: ListMap[String, PathValue],
    @multi @tagged[BodyParam] bodyParams: ListMap[String, BodyValue]
  ): Future[RestResponse]

  @multi
  @tagged[PUT]
  def putSingle(name: String)(
    @multi @tagged[Path] pathParams: List[PathValue],
    @multi @tagged[Header] headerParams: ListMap[String, HeaderValue],
    @multi @tagged[QueryParam] urlParams: ListMap[String, PathValue],
    @encoded @tagged[Body] body: BodyValue
  ): Future[RestResponse]

  @multi
  @tagged[DELETE]
  def delete(name: String)(
    @multi @tagged[Path] pathParams: List[PathValue],
    @multi @tagged[Header] headerParams: ListMap[String, HeaderValue],
    @multi @tagged[QueryParam] urlParams: ListMap[String, PathValue],
    @multi @tagged[BodyParam] bodyParams: ListMap[String, BodyValue]
  ): Future[RestResponse]

  @multi
  @tagged[DELETE]
  def deleteSingle(name: String)(
    @multi @tagged[Path] pathParams: List[PathValue],
    @multi @tagged[Header] headerParams: ListMap[String, HeaderValue],
    @multi @tagged[QueryParam] urlParams: ListMap[String, PathValue],
    @encoded @tagged[Body] body: BodyValue
  ): Future[RestResponse]
}
object RawRest extends RawRpcCompanion[RawRest]
