package com.avsystem.commons
package jetty.rpc

import java.util.regex.Pattern

import com.avsystem.commons.rest.{HeaderValue, HttpBody, HttpRestMethod, PathValue, QueryValue, RestHeaders, RestRequest, RestResponse}
import javax.servlet.http.{HttpServlet, HttpServletRequest, HttpServletResponse}
import org.eclipse.jetty.http.{HttpHeader, HttpStatus, MimeTypes}

class RestServlet(handleRequest: RestRequest => Future[RestResponse]) extends HttpServlet {
  override def service(req: HttpServletRequest, resp: HttpServletResponse): Unit = {
    RestServlet.handle(handleRequest, req, resp)
  }
}

object RestServlet {
  val separatorPattern: Pattern = Pattern.compile("/")

  def handle(
    handleRequest: RestRequest => Future[RestResponse],
    request: HttpServletRequest,
    response: HttpServletResponse
  ): Unit = {
    val method = HttpRestMethod.byName(request.getMethod)

    val path = separatorPattern
      .splitAsStream(request.getPathInfo)
      .asScala
      .skip(1)
      .map(PathValue)
      .to[List]

    val headersBuilder = IListMap.newBuilder[String, HeaderValue]
    request.getHeaderNames.asScala.foreach { headerName =>
      headersBuilder += headerName -> HeaderValue(request.getHeader(headerName))
    }
    val headers = headersBuilder.result()

    val queryBuilder = IListMap.newBuilder[String, QueryValue]
    request.getParameterNames.asScala.foreach { parameterName =>
      queryBuilder += parameterName -> QueryValue(request.getParameter(parameterName))
    }
    val query = queryBuilder.result()

    val bodyReader = request.getReader
    val bodyBuilder = new JStringBuilder
    Iterator.continually(bodyReader.read())
      .takeWhile(_ != -1)
      .foreach(bodyBuilder.appendCodePoint)
    val body = HttpBody(bodyBuilder.toString, MimeTypes.getContentTypeWithoutCharset(request.getContentType))

    val restRequest = RestRequest(method, RestHeaders(path, headers, query), body)

    val asyncContext = request.startAsync()
    handleRequest(restRequest).catchFailures.andThenNow {
      case Success(restResponse) =>
        response.setStatus(restResponse.code)
        response.addHeader(HttpHeader.CONTENT_TYPE.asString(), s"${restResponse.body.mimeType};charset=utf-8")
        response.getWriter.write(restResponse.body.value)
      case Failure(e) =>
        response.setStatus(HttpStatus.INTERNAL_SERVER_ERROR_500)
        response.addHeader(HttpHeader.CONTENT_TYPE.asString(), MimeTypes.Type.TEXT_PLAIN_UTF_8.asString())
        response.getWriter.write(e.getMessage)
    }.andThenNow { case _ => asyncContext.complete() }
  }
}
