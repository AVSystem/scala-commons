package com.avsystem.commons
package jetty.rpc

import javax.servlet.http.{HttpServletRequest, HttpServletResponse}

import com.avsystem.commons.rpc.{RawInvocation, RawRPC}
import org.eclipse.jetty.http.{HttpMethods, HttpStatus}
import org.eclipse.jetty.server.Request
import org.eclipse.jetty.server.handler.AbstractHandler
import upickle.Js

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

/**
  * @author MKej
  */
class RPCHandler(rootRpc: RawRPC)(implicit ec: ExecutionContext) extends AbstractHandler {

  import RPCHandler._

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

  type InvokeFunction[T] = (RawRPC, String, List[List[Js.Value]]) => T

  def invoke[T](path: String, content: String)(f: InvokeFunction[T]): T = {
    val parts = path.split('/')
    val targetRpc = parts.dropRight(1).foldLeft(rootRpc)(_.get(_, Nil))
    val rpcName = parts.last
    val args = jsonToArgs(content)
    f(targetRpc, rpcName, args)
  }

  def handlePost(path: String, content: String): Future[String] =
    invoke(path, content) { (rpc, name, args) =>
      rpc.call(name, args).map(upickle.json.write)
    }

  def handlePut(path: String, content: String): Unit = {
    invoke(path, content) { (rpc, name, args) =>
      rpc.fire(name, args)
    }
  }
}

object RPCHandler {
  def jsonToArgs(json: String): List[List[Js.Value]] = RawInvocation.jsArrToArgs(upickle.json.read(json))
}
