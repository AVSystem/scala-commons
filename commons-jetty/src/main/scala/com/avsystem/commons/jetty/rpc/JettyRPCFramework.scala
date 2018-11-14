package com.avsystem.commons
package jetty.rpc

import java.nio.charset.StandardCharsets

import com.avsystem.commons.annotation.bincompat
import com.avsystem.commons.rpc.StandardRPCFramework
import com.avsystem.commons.serialization.json.{JsonStringInput, JsonStringOutput, RawJson}
import com.avsystem.commons.serialization.{GenCodec, HasGenCodec}
import com.typesafe.scalalogging.LazyLogging
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}
import org.eclipse.jetty.client.HttpClient
import org.eclipse.jetty.client.api.Result
import org.eclipse.jetty.client.util.{BufferingResponseListener, StringContentProvider}
import org.eclipse.jetty.http.{HttpMethod, HttpStatus, MimeTypes}
import org.eclipse.jetty.server.handler.AbstractHandler
import org.eclipse.jetty.server.{Handler, Request}

import scala.concurrent.duration._

object JettyRPCFramework extends StandardRPCFramework with LazyLogging {
  class RawValue(val s: String) extends AnyVal

  override type Reader[T] = GenCodec[T]
  override type Writer[T] = GenCodec[T]
  override type ParamTypeMetadata[T] = ClassTag[T]
  override type ResultTypeMetadata[T] = DummyImplicit

  private implicit val rawValueCodec: GenCodec[RawValue] = GenCodec.create(
    i => new RawValue(i.readCustom(RawJson).getOrElse(i.readSimple().readString())),
    (o, v) => if (!o.writeCustom(RawJson, v.s)) o.writeSimple().writeString(v.s)
  )

  override def read[T: Reader](raw: RawValue): T = JsonStringInput.read[T](raw.s)
  override def write[T: Writer](value: T): RawValue = new RawValue(JsonStringOutput.write[T](value))

  case class Call(chain: List[RawInvocation], leaf: RawInvocation)
  object Call extends HasGenCodec[Call]

  class RPCClient(httpClient: HttpClient, uri: String, maxResponseLength: Int) {
    @bincompat private[commons]
    def this(httpClient: HttpClient, uri: String, maxResponseLength: Int, ec: ExecutionContext) =
      this(httpClient, uri, maxResponseLength)

    private class RawRPCImpl(chain: List[RawInvocation]) extends RawRPC {
      override def fire(invocation: RawInvocation): Unit =
        put(Call(chain, invocation))

      override def call(invocation: RawInvocation): Future[RawValue] =
        post(Call(chain, invocation))

      override def get(invocation: RawInvocation): RawRPC =
        new RawRPCImpl(chain :+ invocation)
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

  class RPCHandler(rootRpc: RawRPC, contextTimeout: FiniteDuration) extends AbstractHandler {
    @bincompat private[commons]
    def this(rootRpc: RawRPC, contextTimeout: FiniteDuration, ec: ExecutionContext) =
      this(rootRpc, contextTimeout)

    override def handle(target: String, baseRequest: Request, request: HttpServletRequest, response: HttpServletResponse): Unit = {
      baseRequest.setHandled(true)

      val content = Iterator.continually(request.getReader.readLine())
        .takeWhile(_ != null)
        .mkString("\n")

      val call = read[Call](new RawValue(content))

      HttpMethod.fromString(request.getMethod) match {
        case HttpMethod.POST =>
          val async = request.startAsync().setup(_.setTimeout(contextTimeout.toMillis))
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

    type InvokeFunction[T] = RawRPC => RawInvocation => T

    def invoke[T](call: Call)(f: InvokeFunction[T]): T = {
      val rpc = call.chain.foldLeft(rootRpc)((rpc, inv) => rpc.get(inv))
      f(rpc)(call.leaf)
    }

    def handlePost(call: Call): Future[RawValue] =
      invoke(call)(_.call)

    def handlePut(call: Call): Unit =
      invoke(call)(_.fire)
  }

  def newHandler[T](impl: T, contextTimeout: FiniteDuration = 30.seconds)(
    implicit asRawRPC: AsRawRPC[T]): Handler =
    new RPCHandler(asRawRPC.asRaw(impl), contextTimeout)

  @bincompat private[commons]
  def newHandler[T: AsRawRPC](impl: T, contextTimeout: FiniteDuration, ec: ExecutionContext): Handler =
    newHandler(impl, contextTimeout)

  def newClient[T](httpClient: HttpClient, uri: String, maxResponseLength: Int = 2 * 1024 * 1024)(
    implicit asRealRPC: AsRealRPC[T]): T =
    asRealRPC.asReal(new RPCClient(httpClient, uri, maxResponseLength).rawRPC)

  @bincompat private[commons]
  def newClient[T: AsRealRPC](httpClient: HttpClient, uri: String, maxResponseLength: Int, ec: ExecutionContext): T =
    newClient[T](httpClient, uri, maxResponseLength)
}
