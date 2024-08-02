package com.avsystem.commons
package jetty.rpc

import com.avsystem.commons.rpc.StandardRPCFramework
import com.avsystem.commons.serialization.json.{JsonStringInput, JsonStringOutput, RawJson}
import com.avsystem.commons.serialization.{GenCodec, HasGenCodec}
import com.typesafe.scalalogging.LazyLogging
import jakarta.servlet.http.{HttpServlet, HttpServletRequest, HttpServletResponse}
import org.eclipse.jetty.client.{BufferingResponseListener, HttpClient, Result, StringRequestContent}
import org.eclipse.jetty.ee10.servlet.ServletContextHandler
import org.eclipse.jetty.http.{HttpMethod, HttpStatus, MimeTypes}
import org.eclipse.jetty.server.Handler

import java.nio.charset.StandardCharsets
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.duration.*
import scala.util.Using

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
      val promise = Promise[RawValue]()

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

      val content = new StringRequestContent(
        MimeTypes.Type.APPLICATION_JSON.asString(),
        write(call).s,
        StandardCharsets.UTF_8
      )

      httpClient.newRequest(uri)
        .method(method)
        .body(content)
        .send(listener)

      promise.future
    }

    def post(call: Call): Future[RawValue] =
      request(HttpMethod.POST, call)

    def put(call: Call): Unit =
      request(HttpMethod.PUT, call)
  }

  class RPCHandler(rootRpc: RawRPC, contextTimeout: FiniteDuration) extends HttpServlet {
    override def service(request: HttpServletRequest, response: HttpServletResponse): Unit = {
      // readRequest must execute in request thread but we want exceptions to be handled uniformly, hence the Try
      val content =
        Using(request.getReader)(reader =>
          Iterator.continually(reader.readLine()).takeWhile(_ != null).mkString("\n")
        )
      val call = content.map(content => read[Call](new RawValue(content)))

      HttpMethod.fromString(request.getMethod) match {
        case HttpMethod.POST =>
          val asyncContext = request.startAsync().setup(_.setTimeout(contextTimeout.toMillis))
          val completed = new AtomicBoolean(false)
          // Need to protect asyncContext from being completed twice because after a timeout the
          // servlet may recycle the same context instance between subsequent requests (not cool)
          // https://stackoverflow.com/a/27744537
          def completeWith(code: => Unit): Unit =
            if (!completed.getAndSet(true)) {
              code
              asyncContext.complete()
            }
          completeWith(Future.fromTry(call).flatMapNow(handlePost).andThenNow {
            case Success(responseContent) =>
              response.setContentType(MimeTypes.Type.APPLICATION_JSON.asString())
              response.setCharacterEncoding(StandardCharsets.UTF_8.name())
              response.getWriter.write(responseContent.s)
            case Failure(t) =>
              response.sendError(HttpStatus.INTERNAL_SERVER_ERROR_500, t.getMessage)
              logger.error("Failed to handle RPC call", t)
          })
        case HttpMethod.PUT =>
          call.map(handlePut).get
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

  def newServlet[T: AsRawRPC](impl: T, contextTimeout: FiniteDuration = 30.seconds): HttpServlet =
    new RPCHandler(AsRawRPC[T].asRaw(impl), contextTimeout)

  def newHandler[T: AsRawRPC](impl: T, contextTimeout: FiniteDuration = 30.seconds): Handler =
    new ServletContextHandler().setup(_.addServlet(newServlet(impl, contextTimeout), "/*"))

  def newClient[T: AsRealRPC](httpClient: HttpClient, uri: String, maxResponseLength: Int = 2 * 1024 * 1024): T =
    AsRealRPC[T].asReal(new RPCClient(httpClient, uri, maxResponseLength).rawRPC)
}
