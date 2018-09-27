package com.avsystem.commons
package jetty.rest

import com.avsystem.commons.annotation.explicitGenerics
import com.avsystem.commons.jetty.rest.RestServlet.DefaultHandleTimeout
import com.avsystem.commons.meta.Mapping
import com.avsystem.commons.rest._
import javax.servlet.http.{HttpServlet, HttpServletRequest, HttpServletResponse}
import org.eclipse.jetty.http.{HttpStatus, MimeTypes}

import scala.concurrent.duration._

class RestServlet(handleRequest: RawRest.HandleRequest, handleTimeout: FiniteDuration = DefaultHandleTimeout)
  extends HttpServlet {

  override def service(req: HttpServletRequest, resp: HttpServletResponse): Unit =
    RestServlet.handle(handleRequest, req, resp, handleTimeout)
}

object RestServlet {
  final val DefaultHandleTimeout = 30.seconds

  def apply[@explicitGenerics Real: RawRest.AsRawRpc : RestMetadata](
    real: Real, handleTimeout: FiniteDuration = DefaultHandleTimeout
  ): RestServlet =
    new RestServlet(RawRest.asHandleRequest[Real](real))

  def readParameters(request: HttpServletRequest): RestParameters = {
    // can't use request.getPathInfo because it decodes the URL before we can split it
    val path = PathValue.splitDecode(request.getRequestURI
      .stripPrefix(request.getContextPath).stripPrefix(request.getServletPath))
    val query = request.getQueryString.opt.map(QueryValue.decode).getOrElse(Mapping.empty)
    val headersBuilder = Mapping.newBuilder[HeaderValue]
    request.getHeaderNames.asScala.foreach { headerName =>
      headersBuilder += headerName -> HeaderValue(request.getHeader(headerName))
    }
    val headers = headersBuilder.result()
    RestParameters(path, headers, query)
  }

  def readBody(request: HttpServletRequest): HttpBody = {
    val mimeType = request.getContentType.opt.map(MimeTypes.getContentTypeWithoutCharset)
    mimeType.fold(HttpBody.empty) { mimeType =>
      val bodyReader = request.getReader
      val bodyBuilder = new JStringBuilder
      Iterator.continually(bodyReader.read())
        .takeWhile(_ != -1)
        .foreach(bodyBuilder.appendCodePoint)
      HttpBody(bodyBuilder.toString, mimeType)
    }
  }

  def readRequest(request: HttpServletRequest): RestRequest = {
    val method = HttpMethod.byName(request.getMethod)
    val parameters = readParameters(request)
    val body = readBody(request)
    RestRequest(method, parameters, body)
  }

  def writeResponse(response: HttpServletResponse, restResponse: RestResponse, charset: String = "utf-8"): Unit = {
    response.setStatus(restResponse.code)
    restResponse.headers.foreach {
      case (name, HeaderValue(value)) => response.addHeader(name, value)
    }
    restResponse.body.forNonEmpty { (content, mimeType) =>
      response.setContentType(s"$mimeType;charset=$charset")
      response.getWriter.write(content)
    }
  }

  def writeFailure(response: HttpServletResponse, message: String, charset: String = "utf-8"): Unit = {
    response.setStatus(HttpStatus.INTERNAL_SERVER_ERROR_500)
    response.setContentType(s"text/plain;charset=$charset")
    response.getWriter.write(message)
  }

  def handle(
    handleRequest: RawRest.HandleRequest,
    request: HttpServletRequest,
    response: HttpServletResponse,
    handleTimeout: FiniteDuration = DefaultHandleTimeout,
    charset: String = "utf-8"
  ): Unit = {
    val asyncContext = request.startAsync().setup(_.setTimeout(handleTimeout.toMillis))
    RawRest.safeAsync(handleRequest(readRequest(request))) {
      case Success(restResponse) =>
        writeResponse(response, restResponse, charset)
        asyncContext.complete()
      case Failure(e) =>
        writeFailure(response, e.getMessage, charset)
        asyncContext.complete()
    }
  }
}
