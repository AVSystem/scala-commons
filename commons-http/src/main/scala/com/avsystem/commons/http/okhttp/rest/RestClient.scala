package com.avsystem.commons
package http.okhttp.rest

import java.io.IOException
import java.net.URLEncoder

import com.avsystem.commons.annotation.explicitGenerics
import com.avsystem.commons.rest.RawRest.HandleRequest
import com.avsystem.commons.rest.{HeaderValue, HttpBody, PathValue, QueryValue, RawRest, RestMetadata, RestResponse}
import okhttp3.{Call, Callback, HttpUrl, MediaType, OkHttpClient, Request, RequestBody, Response}

object RestClient {

  def apply[@explicitGenerics Real: RawRest.AsRealRpc : RestMetadata](
    client: OkHttpClient,
    scheme: String,
    host: String,
    port: Int,
    basePath: String
  ): Real =
    RawRest.fromHandleRequest[Real](asHandleRequest(client, scheme, host, port, basePath))

  def asHandleRequest(
    client: OkHttpClient,
    scheme: String,
    host: String,
    port: Int,
    basePath: String
  ): HandleRequest = {
    RawRest.safeHandle(request => callback => {
      val url = new HttpUrl.Builder()
        .scheme(scheme)
        .host(host)
        .port(port)
        .addEncodedPathSegment(URLEncoder.encode(basePath, "utf-8"))

      request.parameters.path.foreach {
        case PathValue(value) => url.addEncodedPathSegment(URLEncoder.encode(value, "utf-8"))
      }

      request.parameters.query.foreach {
        case (name, QueryValue(value)) => url.addQueryParameter(name, value)
      }

      val httpReq = new Request.Builder()
        .url(url.build())

      request.parameters.headers.foreach {
        case (name, HeaderValue(value)) => httpReq.addHeader(name, value)
      }

      request.body match {
        case HttpBody(content, mimeType) =>
          val mediaType = MediaType.parse(s"$mimeType;charset=utf-8")
          httpReq.method(request.method.toString, RequestBody.create(mediaType, content))
        case HttpBody.Empty => httpReq.method(request.method.toString, null)
      }

      client.newCall(httpReq.build())
        .enqueue(new Callback() {
          override def onFailure(call: Call, e: IOException): Unit = {
            callback(Failure(e))
          }
          override def onResponse(call: Call, response: Response): Unit = {
            val body = response.headers().get("Content-Type").opt
              .map(MediaType.parse)
              .fold(HttpBody.empty) { contentType =>
                HttpBody(response.body.string, s"${contentType.`type`}/${contentType.subtype}")
              }
            val result = new RestResponse(response.code, body)
            callback(Success(result))
          }
        })

    })

  }

}
