package com.avsystem.commons
package jetty.rpc

import java.net.URLDecoder
import java.util.regex.Pattern

import com.avsystem.commons.rest.{HeaderValue, HttpBody, HttpMethod, PathValue, QueryValue, RawRest, RestHeaders, RestRequest}
import com.avsystem.commons.rpc.NamedParams
import javax.servlet.http.{HttpServlet, HttpServletRequest, HttpServletResponse}
import org.eclipse.jetty.http.{HttpStatus, MimeTypes}

class RestServlet(handleRequest: RawRest.HandleRequest) extends HttpServlet {
  override def service(req: HttpServletRequest, resp: HttpServletResponse): Unit = {
    RestServlet.handle(handleRequest, req, resp)
  }
}

object RestServlet {
  val separatorPattern: Pattern = Pattern.compile("/")

  def handle(
    handleRequest: RawRest.HandleRequest,
    request: HttpServletRequest,
    response: HttpServletResponse
  ): Unit = {
    val method = HttpMethod.byName(request.getMethod)

    // can't use request.getPathInfo because it decodes the URL before we can split it
    val encodedPath = request.getRequestURI.stripPrefix(request.getServletPath).stripPrefix("/")
    val path = separatorPattern
      .splitAsStream(encodedPath).asScala
      .map(v => PathValue(URLDecoder.decode(v, "utf-8")))
      .to[List]

    val headersBuilder = NamedParams.newBuilder[HeaderValue]
    request.getHeaderNames.asScala.foreach { headerName =>
      headersBuilder += headerName -> HeaderValue(request.getHeader(headerName))
    }
    val headers = headersBuilder.result()

    val queryBuilder = NamedParams.newBuilder[QueryValue]
    request.getParameterNames.asScala.foreach { parameterName =>
      queryBuilder += parameterName -> QueryValue(request.getParameter(parameterName))
    }
    val query = queryBuilder.result()

    val bodyReader = request.getReader
    val bodyBuilder = new JStringBuilder
    Iterator.continually(bodyReader.read())
      .takeWhile(_ != -1)
      .foreach(bodyBuilder.appendCodePoint)
    val bodyString = bodyBuilder.toString
    val body =
      if (bodyString.isEmpty && request.getContentType == null) HttpBody.Empty
      else HttpBody(bodyString, MimeTypes.getContentTypeWithoutCharset(request.getContentType))

    val restRequest = RestRequest(method, RestHeaders(path, headers, query), body)

    val asyncContext = request.startAsync()
    handleRequest(restRequest).catchFailures.andThenNow {
      case Success(restResponse) =>
        response.setStatus(restResponse.code)
        response.setContentLength(restResponse.body.content.length)
        response.setContentType(s"${restResponse.body.mimeType};charset=utf-8")
        response.getWriter.write(restResponse.body.content)
      case Failure(e) =>
        response.setStatus(HttpStatus.INTERNAL_SERVER_ERROR_500)
        response.setContentLength(e.getMessage.length)
        response.setContentType(MimeTypes.Type.TEXT_PLAIN_UTF_8.asString())
        response.getWriter.write(e.getMessage)
    }.andThenNow { case _ => asyncContext.complete() }
  }
}
