package com.avsystem.commons
package jetty.rpc

import com.avsystem.commons.rest.AbstractRestCallTest
import com.avsystem.commons.rest.RawRest.HandleRequest
import org.eclipse.jetty.server.Server
import org.eclipse.jetty.servlet.{ServletHandler, ServletHolder}

import scala.concurrent.duration._

class HttpRestCallTest extends AbstractRestCallTest with UsesHttpServer with UsesHttpClient {
  override def patienceConfig: PatienceConfig = PatienceConfig(10.seconds)

  protected def setupServer(server: Server): Unit = {
    val servlet = new RestServlet(serverHandle)
    val holder = new ServletHolder(servlet)
    val handler = new ServletHandler
    handler.addServletWithMapping(holder, "/api/*")
    server.setHandler(handler)
  }

  def clientHandle: HandleRequest =
    RestClient.asHandleRequest(client, s"$baseUrl/api")
}
