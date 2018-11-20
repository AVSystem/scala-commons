package com.avsystem.commons
package jetty.rest

import java.nio.charset.StandardCharsets

import com.avsystem.commons.annotation.explicitGenerics
import com.avsystem.commons.meta.Mapping
import com.avsystem.commons.rest.{HeaderValue, HttpBody, PathValue, QueryValue, RawRest, RestMetadata, RestResponse}
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
      val path = baseUrl + PathValue.encodeJoin(request.parameters.path)
      val httpReq = client.newRequest(baseUrl).method(request.method.toString)

      httpReq.path(path)
      request.parameters.query.foreach {
        case (name, QueryValue(value)) => httpReq.param(name, value)
      }
      request.parameters.headers.foreach {
        case (name, HeaderValue(value)) => httpReq.header(name, value)
      }

      request.body.forNonEmpty { (content, mimeType) =>
        httpReq.content(new StringContentProvider(s"$mimeType;charset=utf-8", content, StandardCharsets.UTF_8))
      }

      httpReq.send(new BufferingResponseListener(maxResponseLength) {
        override def onComplete(result: Result): Unit =
          if (result.isSucceeded) {
            val httpResp = result.getResponse
            val body = httpResp.getHeaders.get(HttpHeader.CONTENT_TYPE).opt.fold(HttpBody.empty) { contentType =>
              HttpBody(getContentAsString(), MimeTypes.getContentTypeWithoutCharset(contentType))
            }
            val headers = httpResp.getHeaders.iterator.asScala.map(h => (h.getName, HeaderValue(h.getValue))).toList
            val response = RestResponse(httpResp.getStatus, Mapping(headers), body)
            callback(Success(response))
          } else {
            callback(Failure(result.getFailure))
          }
      })
    })
}
