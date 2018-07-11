package com.avsystem.commons
package jetty.rest

import java.net.URLEncoder
import java.nio.charset.StandardCharsets

import com.avsystem.commons.rest.{HeaderValue, HttpBody, QueryValue, RawRest, RestResponse}
import org.eclipse.jetty.client.HttpClient
import org.eclipse.jetty.client.api.Result
import org.eclipse.jetty.client.util.{BufferingResponseListener, StringContentProvider}
import org.eclipse.jetty.http.{HttpHeader, MimeTypes}

object RestClient {
  def asHandleRequest(client: HttpClient, baseUrl: String): RawRest.HandleRequest = request => {
    val path = request.headers.path.iterator
      .map(pv => URLEncoder.encode(pv.value, "utf-8"))
      .mkString(baseUrl.ensureSuffix("/"), "/", "")

    val httpReq = client.newRequest(baseUrl).method(request.method.toString)

    httpReq.path(path)
    request.headers.query.foreach {
      case (name, QueryValue(value)) => httpReq.param(name, value)
    }
    request.headers.headers.foreach {
      case (name, HeaderValue(value)) => httpReq.header(name, value)
    }

    request.body.forNonEmpty { (content, mimeType) =>
      httpReq.content(new StringContentProvider(s"$mimeType;charset=utf-8", content, StandardCharsets.UTF_8))
    }

    val promise = Promise[RestResponse]
    httpReq.send(new BufferingResponseListener() {
      override def onComplete(result: Result): Unit =
        if (result.isSucceeded) {
          val httpResp = result.getResponse
          val body = httpResp.getHeaders.get(HttpHeader.CONTENT_TYPE).opt.fold(HttpBody.empty) { contentType =>
            HttpBody(getContentAsString(), MimeTypes.getContentTypeWithoutCharset(contentType))
          }
          val response = RestResponse(httpResp.getStatus, body)
          promise.success(response)
        } else {
          promise.failure(result.getFailure)
        }
    })

    promise.future
  }
}
