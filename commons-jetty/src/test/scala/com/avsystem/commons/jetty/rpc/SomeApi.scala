package com.avsystem.commons
package jetty.rpc

import com.avsystem.commons.rest.{GET, POST, RawRest, RestApiCompanion, RestRequest, RestResponse}

trait SomeApi {
  @GET
  def hello(who: String): Future[String]

  @POST("hello")
  def helloThere(who: String): Future[String]
}

object SomeApi extends RestApiCompanion[SomeApi] {
  def asHandleRequest(real: SomeApi): RestRequest => Future[RestResponse] =
    RawRest.asHandleRequest(real)

  def fromHandleRequest(handle: RestRequest => Future[RestResponse]): SomeApi =
    RawRest.fromHandleRequest[SomeApi](handle)

  def format(who: String) = s"Hello, $who!"
  val poison: String = "poison"

  val impl: SomeApi = new SomeApi {
    override def hello(who: String): Future[String] = {
      if (who == poison) throw new IllegalArgumentException(poison)
      else Future.successful(format(who))
    }

    override def helloThere(who: String): Future[String] = hello(who)
  }
}
