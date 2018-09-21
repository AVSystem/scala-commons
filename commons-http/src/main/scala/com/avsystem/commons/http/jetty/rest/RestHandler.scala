package com.avsystem.commons
package http.jetty.rest

import com.avsystem.commons.annotation.explicitGenerics
import com.avsystem.commons.rest.{RawRest, RestMetadata}
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}
import org.eclipse.jetty.server.Request
import org.eclipse.jetty.server.handler.AbstractHandler
import RestServlet.DefaultHandleTimeout

import scala.concurrent.duration.FiniteDuration

class RestHandler(handleRequest: RawRest.HandleRequest, handleTimeout: FiniteDuration = DefaultHandleTimeout)
  extends AbstractHandler {

  override def handle(target: String, baseRequest: Request, request: HttpServletRequest, response: HttpServletResponse): Unit = {
    baseRequest.setHandled(true)
    RestServlet.handle(handleRequest, request, response, handleTimeout)
  }
}

object RestHandler {
  def apply[@explicitGenerics Real: RawRest.AsRawRpc : RestMetadata](
    real: Real, handleTimeout: FiniteDuration = DefaultHandleTimeout
  ): RestHandler =
    new RestHandler(RawRest.asHandleRequest[Real](real), handleTimeout)
}
