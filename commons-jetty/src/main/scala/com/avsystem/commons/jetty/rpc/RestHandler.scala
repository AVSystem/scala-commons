package com.avsystem.commons
package jetty.rpc

import com.avsystem.commons.rest.{RestRequest, RestResponse}
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}
import org.eclipse.jetty.server.Request
import org.eclipse.jetty.server.handler.AbstractHandler

class RestHandler(handleRequest: RestRequest => Future[RestResponse]) extends AbstractHandler {
  override def handle(target: String, baseRequest: Request, request: HttpServletRequest, response: HttpServletResponse): Unit = {
    baseRequest.setHandled(true)
    RestServlet.handle(handleRequest, request, response)
  }
}
