package com.avsystem.commons
package jetty.rest

import java.net.URLDecoder
import java.util.regex.Pattern

import com.avsystem.commons.annotation.explicitGenerics
import com.avsystem.commons.meta.Mapping
import com.avsystem.commons.rest.{HeaderValue, HttpBody, HttpMethod, PathValue, QueryValue, RawRest, RestMetadata, RestParameters, RestRequest}
import javax.servlet.http.{HttpServlet, HttpServletRequest, HttpServletResponse}
import org.eclipse.jetty.http.{HttpStatus, MimeTypes}

class RestServlet(handleRequest: RawRest.HandleRequest) extends HttpServlet {
  override def service(req: HttpServletRequest, resp: HttpServletResponse): Unit = {
    RestServlet.handle(handleRequest, req, resp)
  }
}

object RestServlet {
  def apply[@explicitGenerics Real: RawRest.AsRawRpc : RestMetadata](real: Real): RestServlet =
    new RestServlet(RawRest.asHandleRequest[Real](real))

  private val SeparatorPattern: Pattern = Pattern.compile("/")

  def handle(
    handleRequest: RawRest.HandleRequest,
    request: HttpServletRequest,
    response: HttpServletResponse
  ): Unit = {
    val method = HttpMethod.byName(request.getMethod)

    // can't use request.getPathInfo because it decodes the URL before we can split it
    val encodedPath = request.getRequestURI.stripPrefix(request.getServletPath).stripPrefix("/")
    val path = SeparatorPattern
      .splitAsStream(encodedPath).asScala
      .map(v => PathValue(URLDecoder.decode(v, "UTF-8")))
      .to[List]

    val headersBuilder = Mapping.newBuilder[HeaderValue]
    request.getHeaderNames.asScala.foreach { headerName =>
      headersBuilder += headerName -> HeaderValue(request.getHeader(headerName))
    }
    val headers = headersBuilder.result()

    val mimeType = request.getContentType.opt.map(MimeTypes.getContentTypeWithoutCharset)
    val queryBuilder = Mapping.newBuilder[QueryValue]
    if (!mimeType.contains(HttpBody.FormType)) {
      request.getParameterNames.asScala.foreach { parameterName =>
        queryBuilder += parameterName -> QueryValue(request.getParameter(parameterName))
      }
    }
    val query = queryBuilder.result()

    val body = mimeType.fold(HttpBody.empty) { mimeType =>
      val bodyReader = request.getReader
      val bodyBuilder = new JStringBuilder
      Iterator.continually(bodyReader.read())
        .takeWhile(_ != -1)
        .foreach(bodyBuilder.appendCodePoint)
      HttpBody(bodyBuilder.toString, mimeType)
    }
    val restRequest = RestRequest(method, RestParameters(path, headers, query), body)

    val asyncContext = request.startAsync()
    RawRest.safeAsync(handleRequest(restRequest)) {
      case Success(restResponse) =>
        response.setStatus(restResponse.code)
        restResponse.body.forNonEmpty { (content, mimeType) =>
          response.setContentType(s"$mimeType;charset=utf-8")
          response.getWriter.write(content)
        }
        asyncContext.complete()
      case Failure(e) =>
        response.setStatus(HttpStatus.INTERNAL_SERVER_ERROR_500)
        response.setContentType(MimeTypes.Type.TEXT_PLAIN_UTF_8.asString())
        response.getWriter.write(e.getMessage)
        asyncContext.complete()
    }
  }
}
