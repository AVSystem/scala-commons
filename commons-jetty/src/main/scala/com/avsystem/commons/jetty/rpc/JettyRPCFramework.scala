package com.avsystem.commons
package jetty.rpc

import java.nio.charset.StandardCharsets

import com.avsystem.commons.rpc.StandardRPCFramework
import com.avsystem.commons.serialization.GenCodec
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}
import org.eclipse.jetty.client.HttpClient
import org.eclipse.jetty.client.api.Result
import org.eclipse.jetty.client.util.{BufferingResponseListener, BytesContentProvider}
import org.eclipse.jetty.http.{HttpMethod, HttpStatus}
import org.eclipse.jetty.server.handler.AbstractHandler
import org.eclipse.jetty.server.{Handler, Request}

/**
  * @author MKej
  */
trait JettyRPCFramework extends StandardRPCFramework {
  type RawValue = String
  type Reader[T] = GenCodec[T]
  type Writer[T] = GenCodec[T]

  class RPCClient(httpClient: HttpClient, urlPrefix: String)(implicit ec: ExecutionContext) {
    private class RawRPCImpl(pathPrefix: String) extends RawRPC {
      def fire(rpcName: String, argLists: List[List[RawValue]]): Unit =
        put(pathPrefix + rpcName, write(argLists))

      def call(rpcName: String, argLists: List[List[RawValue]]): Future[RawValue] =
        post(pathPrefix + rpcName, write(argLists))

      def get(rpcName: String, argLists: List[List[RawValue]]): RawRPC = argLists match {
        case Nil => new RawRPCImpl(s"$pathPrefix$rpcName/")
        case _ => throw new IllegalArgumentException("Only no-arg list sub-RPCs are supported (without parentheses)")
      }
    }

    val rawRPC: RawRPC = new RawRPCImpl("")

    def request(method: HttpMethod, path: String, content: String): Future[String] = {
      val promise = Promise[String]
      httpClient.newRequest(urlPrefix + path)
        .method(method)
        .content(new BytesContentProvider(content.getBytes(StandardCharsets.UTF_8)))
        .send(new BufferingResponseListener() {
        override def onComplete(result: Result): Unit = {
          if (result.isFailed) {
            promise.tryFailure(result.getFailure)
          } else {
            val response = result.getResponse
            if (HttpStatus.isSuccess(response.getStatus)) {
              promise.success(getContentAsString)
            } else {
              promise.failure(new HttpException(response.getStatus, response.getReason))
            }
          }
        }
      })

      promise.future
    }

    def post(path: String, content: String): Future[String] =
      request(HttpMethod.POST, path, content)

    def put(path: String, content: String): Unit =
      request(HttpMethod.PUT, path, content)

  }

  class RPCHandler(rootRpc: RawRPC)(implicit ec: ExecutionContext) extends AbstractHandler {

    def handle(target: String, baseRequest: Request, request: HttpServletRequest, response: HttpServletResponse): Unit = {
      baseRequest.setHandled(true)

      val cleanTarget = target.stripPrefix("/").stripSuffix("/")

      val content = Iterator.continually(request.getReader.readLine())
        .takeWhile(_ != null)
        .mkString("\n")

      HttpMethod.fromString(request.getMethod) match {
        case HttpMethod.POST =>
          val async = request.startAsync()
          handlePost(cleanTarget, content).andThenNow {
            case Success(responseContent) =>
              response.getWriter.write(responseContent)
            case Failure(t) =>
              response.sendError(HttpStatus.INTERNAL_SERVER_ERROR_500, t.getMessage)
          }.andThenNow { case _ => async.complete() }
        case HttpMethod.PUT =>
          handlePut(cleanTarget, content)
        case _ =>
          throw new IllegalArgumentException(s"Request HTTP method is ${request.getMethod}, only POST or PUT are supported")
      }
    }

    type InvokeFunction[T] = (RawRPC, String, List[List[RawValue]]) => T

    def invoke[T](path: String, content: String)(f: InvokeFunction[T]): T = {
      val parts = path.split('/')
      val targetRpc = parts.dropRight(1).foldLeft(rootRpc)(_.get(_, Nil))
      val rpcName = parts.last
      val args = read[List[List[RawValue]]](content)
      f(targetRpc, rpcName, args)
    }

    def handlePost(path: String, content: String): Future[String] =
      invoke(path, content) { (rpc, name, args) =>
        rpc.call(name, args)
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
