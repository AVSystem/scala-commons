package com.avsystem.commons
package okhttp.rest

import com.avsystem.commons.rest.{DefaultRestApiCompanion, GET, Header, POST, Path, Query}

trait SomeApi {

  @GET
  def hello(who: String): Future[String]

  @POST
  def helloThere(who: String): Future[String]

  @GET
  def helloWithHeader(@Header("who") who: String): Future[String]

  @GET
  def helloWithQueryParam(@Query who: String): Future[String]

  @GET
  def helloWithPathParam(@Path who: String): Future[String]

}

object SomeApi extends DefaultRestApiCompanion[SomeApi] {
  def format(who: String) = s"Hello, $who!"
  val poison = "poison"
}

class SomeApiImpl extends SomeApi {

  override def hello(who: String): Future[String] = {
    if (who == SomeApi.poison) throw new IllegalArgumentException(SomeApi.poison)
    else Future.successful(SomeApi.format(who))
  }

  override def helloThere(who: String): Future[String] = hello(who)

  override def helloWithHeader(who: String): Future[String] = hello(who)

  override def helloWithQueryParam(who: String): Future[String] = hello(who)

  override def helloWithPathParam(who: String): Future[String] = hello(who)
}