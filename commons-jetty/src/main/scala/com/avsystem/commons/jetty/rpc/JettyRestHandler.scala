package com.avsystem.commons
package jetty.rpc

import java.util.regex.Pattern

import com.avsystem.commons.rest.{BodyValue, HeaderValue, HttpRestMethod, PathValue, QueryValue, RestHeaders, RestRequest, RestResponse}
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}
import org.eclipse.jetty.http.{HttpHeader, HttpStatus, MimeTypes}
import org.eclipse.jetty.server.Request
import org.eclipse.jetty.server.handler.AbstractHandler

class JettyRestHandler(handleRequest: RestRequest => Future[RestResponse]) extends AbstractHandler {

  import JettyRestHandler._

  override def handle(target: String, baseRequest: Request, request: HttpServletRequest, response: HttpServletResponse): Unit = {
    val method = HttpRestMethod.byName(baseRequest.getMethod)

    val path = separatorPattern
      .splitAsStream(baseRequest.getPathInfo)
      .asScala
      .skip(1)
      .map(PathValue)
      .to[List]

    val headersBuilder = IListMap.newBuilder[String, HeaderValue]
    baseRequest.getHeaderNames.asScala.foreach { headerName =>
      headersBuilder += headerName -> HeaderValue(baseRequest.getHeader(headerName))
    }
    val headers = headersBuilder.result()

    val queryBuilder = IListMap.newBuilder[String, QueryValue]
    baseRequest.getParameterNames.asScala.foreach { parameterName =>
      queryBuilder += parameterName -> QueryValue(baseRequest.getParameter(parameterName))
    }
    val query = queryBuilder.result()

    val bodyReader = baseRequest.getReader
    val bodyBuilder = new JStringBuilder
    Iterator.continually(bodyReader.read())
      .takeWhile(_ != -1)
      .foreach(bodyBuilder.append)
    val body = BodyValue(bodyBuilder.toString)

    val restRequest = RestRequest(method, RestHeaders(path, headers, query), body)

    baseRequest.setHandled(true)
    val asyncContext = request.startAsync()
    handleRequest(restRequest).andThenNow {
      case Success(restResponse) =>
        response.setStatus(restResponse.code)
        response.addHeader(HttpHeader.CONTENT_TYPE.asString(), MimeTypes.Type.APPLICATION_JSON_UTF_8.asString())
        response.getWriter.write(restResponse.body.value)
      case Failure(e) =>
        response.setStatus(HttpStatus.INTERNAL_SERVER_ERROR_500)
        response.addHeader(HttpHeader.CONTENT_TYPE.asString(), MimeTypes.Type.TEXT_PLAIN_UTF_8.asString())
        response.getWriter.write(e.getMessage)
    }.andThenNow { case _ => asyncContext.complete() }
  }
}

object JettyRestHandler {
  val separatorPattern: Pattern = Pattern.compile("/")
}
