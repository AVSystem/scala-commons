package com.avsystem.commons
package okhttp.rest

import java.io.IOException

import com.avsystem.commons.annotation.explicitGenerics
import com.avsystem.commons.rest.RawRest.HandleRequest
import com.avsystem.commons.rest.{HeaderValue, HttpBody, RawRest, RestMetadata, RestResponse}
import okhttp3.{Call, Callback, HttpUrl, MediaType, OkHttpClient, Request, RequestBody, Response}

object RestClient {

  def apply[@explicitGenerics Real: RawRest.AsRealRpc : RestMetadata]
  (client: OkHttpClient, scheme: String, host: String, port: Int):Real = RawRest.fromHandleRequest[Real](asHandleRequest(client, scheme, host, port))

  def asHandleRequest(client: OkHttpClient, scheme: String, host: String, port: Int): HandleRequest = {
    RawRest.safeHandle(request => callback => {
      val url = new HttpUrl.Builder()
        .scheme(scheme)
        .host(host)
        .port(port)

      request.parameters.path.iterator.foreach(v => url.addPathSegment(v.value))
      request.parameters.query.iterator.foreach(v => url.addQueryParameter(v._1, v._2.value))

      val httpReq = new Request.Builder()
          .url(url.build())

      request.parameters.headers.foreach {
        case (name, HeaderValue(value)) => httpReq.addHeader(name, value)
      }

      request.body match {
        case HttpBody(content, mimeType) =>
          httpReq.method(request.method.toString, RequestBody.create(MediaType.parse(mimeType), content))
        case HttpBody.Empty => httpReq.method(request.method.toString, null)
      }

      client.newCall(httpReq.build())
        .enqueue(new Callback() {
          override def onFailure(call: Call, e: IOException): Unit = {
            callback(Failure(e))
          }
          override def onResponse(call: Call, response: Response): Unit = {
            if(response.isSuccessful) {
              val body = response.headers()
                .get("Content-Type")
                .opt
                .map(MediaType.parse)
                .fold(HttpBody.empty) { contentType =>
                HttpBody(response.body().string(), s"${contentType.`type`()}/${contentType.subtype()}")
              }
              val result = new RestResponse(response.code(), body)
              callback(Success(result))
            } else {
              callback(Failure(new Exception(response.message())))
            }
          }
        })

    })

  }

}
