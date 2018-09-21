package com.avsystem.commons
package http.jetty.rest

import java.net.URLEncoder
import java.nio.charset.StandardCharsets

import com.avsystem.commons.annotation.explicitGenerics
import com.avsystem.commons.rest.{HeaderValue, HttpBody, QueryValue, RawRest, RestMetadata, RestResponse}
import org.eclipse.jetty.client.HttpClient
import org.eclipse.jetty.client.api.Result
import org.eclipse.jetty.client.util.{BufferingResponseListener, StringContentProvider}
import org.eclipse.jetty.http.{HttpHeader, MimeTypes}

object RestClient {
  final val DefaultMaxResponseLength = 2 * 1024 * 1024

  def apply[@explicitGenerics Real: RawRest.AsRealRpc : RestMetadata](
    client: HttpClient, baseUrl: String, maxResponseLength: Int = DefaultMaxResponseLength
  ): Real = RawRest.fromHandleRequest[Real](asHandleRequest(client, baseUrl, maxResponseLength))

  def asHandleRequest(
    client: HttpClient, baseUrl: String, maxResponseLength: Int = DefaultMaxResponseLength
  ): RawRest.HandleRequest =
    RawRest.safeHandle(request => callback => {
      val path = request.parameters.path.iterator
        .map(pv => URLEncoder.encode(pv.value, "utf-8"))
        .mkString(baseUrl.ensureSuffix("/"), "/", "")

      val httpReq = client.newRequest(baseUrl).method(request.method.toString)

      httpReq.path(path)
      request.parameters.query.foreach {
        case (name, QueryValue(value)) => httpReq.param(name, value)
      }
      request.parameters.headers.foreach {
        case (name, HeaderValue(value)) => httpReq.header(name, value)
      }

      request.body.forNonEmpty { (content, mimeType) =>
        httpReq.content(new StringContentProvider(s"$mimeType;charset=utf-8",
          content, StandardCharsets.UTF_8))
      }

      httpReq.send(new BufferingResponseListener(maxResponseLength) {
        override def onComplete(result: Result): Unit =
          if (result.isSucceeded) {
            val httpResp = result.getResponse
            val body = httpResp.getHeaders.get(HttpHeader.CONTENT_TYPE).opt.fold(HttpBody.empty) { contentType =>
              HttpBody(getContentAsString(), MimeTypes.getContentTypeWithoutCharset(contentType))
            }
            val response = RestResponse(httpResp.getStatus, body)
            callback(Success(response))
          } else {
            callback(Failure(result.getFailure))
          }
      })
    })
}
