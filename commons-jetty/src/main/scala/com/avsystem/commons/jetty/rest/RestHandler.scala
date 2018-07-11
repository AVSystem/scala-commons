package com.avsystem.commons
package jetty.rest

import com.avsystem.commons.rest.RawRest
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}
import org.eclipse.jetty.server.Request
import org.eclipse.jetty.server.handler.AbstractHandler

class RestHandler(handleRequest: RawRest.HandleRequest) extends AbstractHandler {
  override def handle(target: String, baseRequest: Request, request: HttpServletRequest, response: HttpServletResponse): Unit = {
    baseRequest.setHandled(true)
    RestServlet.handle(handleRequest, request, response)
  }
}
