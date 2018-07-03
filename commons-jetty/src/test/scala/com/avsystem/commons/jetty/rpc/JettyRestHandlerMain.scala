package com.avsystem.commons
package jetty.rpc

import com.avsystem.commons.rest.{GET, POST, Query, RawRest, RestMetadata}
import com.avsystem.commons.rpc.rpcName
import org.eclipse.jetty.server.Server

object JettyRestHandlerMain {
  trait SomeApi {
    @GET
    def hello(@Query who: String): Future[String]

    @POST
    @rpcName("hello")
    def helloThere(who: String): Future[String]
  }
  object SomeApi {
    implicit val asRawReal: RawRest.AsRawRealRpc[SomeApi] = RawRest.materializeAsRawReal
    implicit val metadata: RestMetadata[SomeApi] = RestMetadata.materializeForRpc
  }

  def main(args: Array[String]): Unit = {
    val someApiImpl = new SomeApi {
      override def hello(who: String): Future[String] = Future.successful(s"Hello, $who!")
      override def helloThere(who: String): Future[String] = hello(who)
    }

    val handler = new JettyRestHandler(SomeApi.asRawReal.asRaw(someApiImpl).asHandleRequest(SomeApi.metadata))
    val server = new Server(9090)
    server.setHandler(handler)
    server.start()
    server.join()
  }
}
