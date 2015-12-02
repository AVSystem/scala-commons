package com.avsystem.commons
package jetty.rpc

import java.nio.charset.StandardCharsets

import com.avsystem.commons.rpc.{RawInvocation, RawRPC}
import org.eclipse.jetty.client.{ContentExchange, HttpClient}
import org.eclipse.jetty.http.{HttpException, HttpMethods, HttpStatus}
import org.eclipse.jetty.io.ByteArrayBuffer
import upickle.Js

import scala.concurrent.{ExecutionContext, Future, Promise}

/**
  * @author MKej
  */
class RPCClient(httpClient: HttpClient, urlPrefix: String, pathPrefix: String)(implicit ec: ExecutionContext) extends RawRPC {

  import RPCClient._

  def request(method: String, path: String, content: String): Future[String] = {
    val promise = Promise[String]

    val exchange = new ContentExchange(true) {
      override def onResponseComplete(): Unit = {
        if (HttpStatus.isSuccess(getResponseStatus)) promise.success(getResponseContent)
        else promise.failure(new HttpException(getResponseStatus))
      }

      override def onConnectionFailed(x: Throwable): Unit = promise.tryFailure(x)
      override def onException(x: Throwable): Unit = promise.tryFailure(x)
      override def onExpire(): Unit = promise.tryFailure(new RuntimeException("Exchange expired"))
    }
    exchange.setMethod(method)
    exchange.setRequestContent(new ByteArrayBuffer(content.getBytes(StandardCharsets.UTF_8)))
    exchange.setURL(urlPrefix + path)

    httpClient.send(exchange)

    promise.future
  }

  def post(path: String, content: String): Future[String] =
    request(HttpMethods.POST, path, content)

  def put(path: String, content: String): Unit =
    request(HttpMethods.PUT, path, content)

  def fire(rpcName: String, argLists: List[List[Js.Value]]): Unit =
    put(pathPrefix + rpcName, argsToJson(argLists))

  def call(rpcName: String, argLists: List[List[Js.Value]]): Future[Js.Value] =
    post(pathPrefix + rpcName, argsToJson(argLists)).map(upickle.json.read)

  def get(rpcName: String, argLists: List[List[Js.Value]]): RawRPC = argLists match {
    case Nil => new RPCClient(httpClient, urlPrefix, s"$pathPrefix$rpcName/")
    case _ => throw new IllegalArgumentException("Only no-arg list sub-RPCs are supported (without parenthesis)")
  }
}

object RPCClient {
  def argsToJson(args: List[List[Js.Value]]): String = upickle.json.write(RawInvocation.argsToJsArr(args))
}
