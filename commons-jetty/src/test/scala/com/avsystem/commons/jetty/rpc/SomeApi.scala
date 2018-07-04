package com.avsystem.commons
package jetty.rpc

import com.avsystem.commons.rest.{GET, POST, Query, RawRest, RestApiCompanion, RestRequest, RestResponse}
import com.avsystem.commons.rpc.rpcName

trait SomeApi {
  @GET
  def hello(@Query who: String): Future[String]

  @POST
  @rpcName("hello")
  def helloThere(who: String): Future[String]
}

object SomeApi extends RestApiCompanion[SomeApi] {
  def asHandleRequest(real: SomeApi): RestRequest => Future[RestResponse] =
    RawRest.asHandleRequest(real)

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
