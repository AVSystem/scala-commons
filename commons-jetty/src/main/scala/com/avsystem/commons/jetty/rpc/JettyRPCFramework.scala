package com.avsystem.commons
package jetty.rpc

import java.nio.charset.StandardCharsets

import com.avsystem.commons.rpc.StandardRPCFramework
import com.avsystem.commons.serialization.json.{JsonStringInput, JsonStringOutput}
import com.avsystem.commons.serialization.{GenCodec, HasGenCodec}
import com.typesafe.scalalogging.LazyLogging
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}
import org.eclipse.jetty.client.HttpClient
import org.eclipse.jetty.client.api.Result
import org.eclipse.jetty.client.util.{BufferingResponseListener, StringContentProvider}
import org.eclipse.jetty.http.{HttpMethod, HttpStatus, MimeTypes}
import org.eclipse.jetty.server.handler.AbstractHandler
import org.eclipse.jetty.server.{Handler, Request}

object JettyRPCFramework extends StandardRPCFramework with LazyLogging {
  class RawValue(val s: String) extends AnyVal

  override type Reader[T] = GenCodec[T]
  override type Writer[T] = GenCodec[T]
  override type ParamTypeMetadata[T] = ClassTag[T]
  override type ResultTypeMetadata[T] = DummyImplicit

  private implicit val rawValueCodec: GenCodec[RawValue] = GenCodec.create(
    {
      case jsi: JsonStringInput => new RawValue(jsi.readRawJson())
      case other => new RawValue(other.readString())
    },
    {
      case (jso: JsonStringOutput, v) => jso.writeRawJson(v.s)
      case (other, v) => other.writeString(v.s)
    }
  )

  override def read[T: Reader](raw: RawValue): T = JsonStringInput.read[T](raw.s)
  override def write[T: Writer](value: T): RawValue = new RawValue(JsonStringOutput.write[T](value))

  case class Invocation(rpcName: String, args: List[RawValue])
  object Invocation extends HasGenCodec[Invocation]

  case class Call(chain: List[Invocation], leaf: Invocation)
  object Call extends HasGenCodec[Call]

  class RPCClient(httpClient: HttpClient, uri: String, maxResponseLength: Int)(implicit ec: ExecutionContext) {
    private class RawRPCImpl(chain: List[Invocation]) extends RawRPC {
      override def fire(rpcName: String)(args: List[RawValue]): Unit =
        put(Call(chain, Invocation(rpcName, args)))

      override def call(rpcName: String)(args: List[RawValue]): Future[RawValue] =
        post(Call(chain, Invocation(rpcName, args)))

      override def get(rpcName: String)(args: List[RawValue]): RawRPC =
        new RawRPCImpl(chain :+ Invocation(rpcName, args))
    }

    val rawRPC: RawRPC = new RawRPCImpl(List.empty)

    def request(method: HttpMethod, call: Call): Future[RawValue] = {
      val promise = Promise[RawValue]

      val listener = new BufferingResponseListener(maxResponseLength) {
        override def onComplete(result: Result): Unit = {
          if (result.isFailed) {
            promise.tryFailure(result.getFailure)
          } else {
            val response = result.getResponse
            if (HttpStatus.isSuccess(response.getStatus)) {
              promise.success(new RawValue(getContentAsString(StandardCharsets.UTF_8)))
            } else {
              promise.failure(new HttpException(response.getStatus, response.getReason))
            }
          }
        }
      }

      val contentProvider = new StringContentProvider(
        MimeTypes.Type.APPLICATION_JSON.asString(),
        write(call).s,
        StandardCharsets.UTF_8
      )

      httpClient.newRequest(uri)
        .method(method)
        .content(contentProvider)
        .send(listener)

      promise.future
    }

    def post(call: Call): Future[RawValue] =
      request(HttpMethod.POST, call)

    def put(call: Call): Unit =
      request(HttpMethod.PUT, call)
  }

  class RPCHandler(rootRpc: RawRPC)(implicit ec: ExecutionContext) extends AbstractHandler {
    override def handle(target: String, baseRequest: Request, request: HttpServletRequest, response: HttpServletResponse): Unit = {
      baseRequest.setHandled(true)

      val content = Iterator.continually(request.getReader.readLine())
        .takeWhile(_ != null)
        .mkString("\n")

      val call = read[Call](new RawValue(content))

      HttpMethod.fromString(request.getMethod) match {
        case HttpMethod.POST =>
          val async = request.startAsync()
          handlePost(call).andThenNow {
            case Success(responseContent) =>
              response.getWriter.write(responseContent.s)
            case Failure(t) =>
              response.sendError(HttpStatus.INTERNAL_SERVER_ERROR_500, t.getMessage)
              logger.error("Failed to handle RPC call", t)
          }.andThenNow { case _ => async.complete() }
        case HttpMethod.PUT =>
          handlePut(call)
        case _ =>
          throw new IllegalArgumentException(s"Request HTTP method is ${request.getMethod}, only POST or PUT are supported")
      }
    }

    type InvokeFunction[T] = RawRPC => String => List[RawValue] => T

    def invoke[T](call: Call)(f: InvokeFunction[T]): T = {
      val rpc = call.chain.foldLeft(rootRpc)((rpc, inv) => rpc.get(inv.rpcName)(inv.args))
      f(rpc)(call.leaf.rpcName)(call.leaf.args)
    }

    def handlePost(call: Call): Future[RawValue] =
      invoke(call)(_.call)

    def handlePut(call: Call): Unit =
      invoke(call)(_.fire)
  }

  def newHandler[T](impl: T)(implicit ec: ExecutionContext, asRawRPC: AsRawRPC[T]): Handler =
    new RPCHandler(asRawRPC.asRaw(impl))

  def newClient[T](httpClient: HttpClient, uri: String, maxResponseLength: Int = 2 * 1024 * 1024)(
    implicit ec: ExecutionContext, asRealRPC: AsRealRPC[T]): T =
    asRealRPC.asReal(new RPCClient(httpClient, uri, maxResponseLength).rawRPC)
}
