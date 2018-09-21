package com.avsystem.commons
package http.okhttp.rest

import com.avsystem.commons.http.jetty.rest.{RestServlet, UsesHttpServer}
import com.avsystem.commons.rest.RawRest.HandleRequest
import com.avsystem.commons.rest.{AbstractRestCallTest, RestTestApi}
import org.eclipse.jetty.server.Server
import org.eclipse.jetty.servlet.{ServletHandler, ServletHolder}
import org.scalatest.time.{Seconds, Span}

import scala.concurrent.duration._
import scala.language.postfixOps

class OkHttpHttpRestCallTest extends AbstractRestCallTest
  with UsesHttpServer
  with UsesOkHttpClient {

  override implicit def patienceConfig: PatienceConfig = PatienceConfig(Span(10, Seconds))

  override protected def setupServer(server: Server): Unit = {
    val servlet = RestServlet[RestTestApi](RestTestApi.Impl)
    val holder = new ServletHolder(servlet)
    val handler = new ServletHandler
    handler.addServletWithMapping(holder, "/api/*")
    server.setHandler(handler)
  }

  override def clientHandle: HandleRequest = {
    RestClient.asHandleRequest(client, scheme, host, port, basePath)
  }
}