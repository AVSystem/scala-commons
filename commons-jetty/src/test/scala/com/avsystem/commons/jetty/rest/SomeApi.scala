package com.avsystem.commons
package jetty.rest

import com.avsystem.commons.rest.{GET, POST, RawRest, RestApiCompanion}

trait SomeApi {
  @GET
  def hello(who: String): Future[String]

  @POST("hello")
  def helloThere(who: String): Future[String]
}

object SomeApi extends RestApiCompanion[SomeApi] {
  def asHandleRequest(real: SomeApi): RawRest.HandleRequest =
    RawRest.asHandleRequest(real)

  def fromHandleRequest(handle: RawRest.HandleRequest): SomeApi =
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
