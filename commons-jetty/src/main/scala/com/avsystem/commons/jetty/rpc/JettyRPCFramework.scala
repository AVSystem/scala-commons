package com.avsystem.commons
package jetty.rpc

import java.nio.charset.StandardCharsets
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}

import com.avsystem.commons.rpc.RPCFramework
import org.eclipse.jetty.client.{ContentExchange, HttpClient}
import org.eclipse.jetty.http.{HttpException, HttpMethods, HttpStatus}
import org.eclipse.jetty.io.ByteArrayBuffer
import org.eclipse.jetty.server.handler.AbstractHandler
import org.eclipse.jetty.server.{Handler, Request}

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success}

/**
  * @author MKej
  */
trait JettyRPCFramework extends RPCFramework {
  def valueToJson(value: RawValue): String
  def jsonToValue(json: String): RawValue

  def argsToJson(args: List[List[RawValue]]): String
  def jsonToArgs(json: String): List[List[RawValue]]

  class RPCClient(httpClient: HttpClient, urlPrefix: String)(implicit ec: ExecutionContext) {
    private class RawRPCImpl(pathPrefix: String) extends RawRPC {
      def fire(rpcName: String, argLists: List[List[RawValue]]): Unit =
        put(pathPrefix + rpcName, argsToJson(argLists))

      def call(rpcName: String, argLists: List[List[RawValue]]): Future[RawValue] =
        post(pathPrefix + rpcName, argsToJson(argLists)).map(jsonToValue)

      def get(rpcName: String, argLists: List[List[RawValue]]): RawRPC = argLists match {
        case Nil => new RawRPCImpl(s"$pathPrefix$rpcName/")
        case _ => throw new IllegalArgumentException("Only no-arg list sub-RPCs are supported (without parenthesis)")
      }
    }

    val rawRPC: RawRPC = new RawRPCImpl("")

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

  }

  class RPCHandler(rootRpc: RawRPC)(implicit ec: ExecutionContext) extends AbstractHandler {

    def handle(target: String, baseRequest: Request, request: HttpServletRequest, response: HttpServletResponse): Unit = {
      baseRequest.setHandled(true)

      val cleanTarget = target.stripPrefix("/").stripSuffix("/")

      val reader = request.getReader
      val content = Stream.continually(reader.readLine())
        .takeWhile(_ != null)
        .mkString("\n")

      request.getMethod match {
        case HttpMethods.POST =>
          val async = request.startAsync()
          handlePost(cleanTarget, content).onComplete {
            case Success(responseContent) =>
              async.getResponse.getWriter.write(responseContent)
              async.complete()
            case Failure(t) =>
              async.getResponse match {
                case httpResponse: HttpServletResponse =>
                  httpResponse.setStatus(HttpStatus.INTERNAL_SERVER_ERROR_500)
              }
              async.complete()
          }
        case HttpMethods.PUT =>
          handlePut(cleanTarget, content)
      }
    }

    type InvokeFunction[T] = (RawRPC, String, List[List[RawValue]]) => T

    def invoke[T](path: String, content: String)(f: InvokeFunction[T]): T = {
      val parts = path.split('/')
      val targetRpc = parts.dropRight(1).foldLeft(rootRpc)(_.get(_, Nil))
      val rpcName = parts.last
      val args = jsonToArgs(content)
      f(targetRpc, rpcName, args)
    }

    def handlePost(path: String, content: String): Future[String] =
      invoke(path, content) { (rpc, name, args) =>
        rpc.call(name, args).map(valueToJson)
      }

    def handlePut(path: String, content: String): Unit = {
      invoke(path, content) { (rpc, name, args) =>
        rpc.fire(name, args)
      }
    }
  }

  def newHandler[T](impl: T)(implicit ec: ExecutionContext, asRawRPC: AsRawRPC[T]): Handler =
    new RPCHandler(asRawRPC.asRaw(impl))

  def newClient[T](httpClient: HttpClient, urlPrefix: String)(implicit ec: ExecutionContext, asRealRPC: AsRealRPC[T]): T =
    asRealRPC.asReal(new RPCClient(httpClient, urlPrefix).rawRPC)
}
