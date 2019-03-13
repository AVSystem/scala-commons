package com.avsystem.commons
package rest

import java.util.concurrent.atomic.AtomicBoolean

import com.avsystem.commons.meta._
import com.avsystem.commons.rpc._
import com.avsystem.commons.serialization.GenCodec.ReadFailure

import scala.annotation.implicitNotFound

sealed abstract class RestMethodCall {
  val rpcName: String
  val pathParams: List[PathValue]
  val metadata: RestMethodMetadata[_]
}
case class PrefixCall(rpcName: String, pathParams: List[PathValue], metadata: PrefixMetadata[_]) extends RestMethodCall
case class HttpCall(rpcName: String, pathParams: List[PathValue], metadata: HttpMethodMetadata[_]) extends RestMethodCall

case class ResolvedCall(root: RestMetadata[_], prefixes: List[PrefixCall], finalCall: HttpCall) {
  lazy val pathPattern: List[PathPatternElement] =
    if (prefixes.isEmpty) finalCall.metadata.pathPattern
    else (prefixes.iterator.flatMap(_.metadata.pathPattern.iterator) ++
      finalCall.metadata.pathPattern.iterator).toList

  def method: HttpMethod = finalCall.metadata.method

  def rpcChainRepr: String =
    prefixes.iterator.map(_.rpcName).mkString("", "->", s"->${finalCall.rpcName}")
}

@methodTag[RestMethodTag]
@methodTag[BodyTypeTag]
trait RawRest {

  import RawRest._

  // declaration order of raw methods matters - it determines their priority!

  @multi @tried
  @tagged[Prefix](whenUntagged = new Prefix)
  @tagged[NoBody](whenUntagged = new NoBody)
  @paramTag[RestParamTag](defaultTag = new Path)
  @unmatched("it cannot be translated to a prefix method")
  @unmatchedParam[Body]("prefix methods cannot take @Body parameters")
  def prefix(
    @methodName name: String,
    @composite parameters: RestParameters
  ): Try[RawRest]

  @multi @tried
  @tagged[GET]
  @tagged[NoBody](whenUntagged = new NoBody)
  @paramTag[RestParamTag](defaultTag = new Query)
  @unmatched("it cannot be translated to a HTTP GET method")
  @unmatchedParam[Body]("GET methods cannot take @Body parameters")
  def get(
    @methodName name: String,
    @composite parameters: RestParameters
  ): Async[RestResponse]

  @multi @tried
  @tagged[BodyMethodTag](whenUntagged = new POST)
  @tagged[FormBody]
  @paramTag[RestParamTag](defaultTag = new Body)
  @unmatched("it cannot be translated to a HTTP method with form body")
  def handleForm(
    @methodName name: String,
    @composite parameters: RestParameters,
    @multi @tagged[Body] body: Mapping[QueryValue]
  ): Async[RestResponse]

  @multi @tried
  @tagged[BodyMethodTag](whenUntagged = new POST)
  @tagged[JsonBody](whenUntagged = new JsonBody)
  @paramTag[RestParamTag](defaultTag = new Body)
  @unmatched("it cannot be translated to a HTTP method")
  def handleJson(
    @methodName name: String,
    @composite parameters: RestParameters,
    @multi @tagged[Body] body: Mapping[JsonValue]
  ): Async[RestResponse]

  @multi @tried
  @tagged[BodyMethodTag](whenUntagged = new POST)
  @tagged[CustomBody]
  @paramTag[RestParamTag](defaultTag = new Body)
  @unmatched("it cannot be translated to a HTTP method with custom body")
  @unmatchedParam[Body]("expected exactly one @Body parameter but more than one was found")
  def handleCustom(
    @methodName name: String,
    @composite parameters: RestParameters,
    @encoded @tagged[Body] @unmatched("expected exactly one @Body parameter but none was found") body: HttpBody
  ): Async[RestResponse]

  def asHandleRequest(metadata: RestMetadata[_]): HandleRequest =
    RawRest.resolveAndHandle(metadata)(handleResolved)

  def handleResolved(request: RestRequest, resolved: ResolvedCall): Async[RestResponse] = {
    val RestRequest(method, parameters, body) = request
    val ResolvedCall(_, prefixes, finalCall) = resolved
    val HttpCall(finalRpcName, finalPathParams, finalMetadata) = finalCall

    def handleBadBody[T](expr: => T): T = try expr catch {
      case rf: ReadFailure => throw new InvalidRpcCall(s"Invalid HTTP body: ${rf.getMessage}", rf)
    }

    def resolveCall(rawRest: RawRest, prefixes: List[PrefixCall]): Async[RestResponse] = prefixes match {
      case PrefixCall(rpcName, pathParams, _) :: tail =>
        rawRest.prefix(rpcName, parameters.copy(path = pathParams)) match {
          case Success(nextRawRest) => resolveCall(nextRawRest, tail)
          case Failure(e: HttpErrorException) => RawRest.successfulAsync(e.toResponse)
          case Failure(cause) => RawRest.failingAsync(cause)
        }
      case Nil =>
        val finalParameters = parameters.copy(path = finalPathParams)
        if (method == HttpMethod.GET)
          rawRest.get(finalRpcName, finalParameters)
        else if (finalMetadata.customBody)
          rawRest.handleCustom(finalRpcName, finalParameters, body)
        else if (finalMetadata.formBody)
          rawRest.handleForm(finalRpcName, finalParameters, handleBadBody(HttpBody.parseFormBody(body)))
        else
          rawRest.handleJson(finalRpcName, finalParameters, handleBadBody(HttpBody.parseJsonBody(body)))
    }
    try resolveCall(this, prefixes) catch {
      case e: InvalidRpcCall =>
        RawRest.successfulAsync(RestResponse.plain(400, e.getMessage))
    }
  }
}

object RawRest extends RawRpcCompanion[RawRest] {
  /**
    * A callback that gets notified when value of type `T` gets computed or when computation of that value fails.
    * Callbacks should never throw exceptions. Preferably, they should be simple notifiers that delegate the real
    * work somewhere else, e.g. schedule some handling code on a separate executor
    * (e.g. [[scala.concurrent.ExecutionException ExecutionContext]]).
    */
  type Callback[T] = Try[T] => Unit

  /**
    * The most low-level, raw type representing an asynchronous, possibly side-effecting operation that yields a
    * value of type `T` as a result.
    * `Async` is a consumer of a callback. When a callback is passed to `Async`, it should start the operation
    * and ultimately notify the callback about the result. Each time the callback is passed, the
    * entire operation should be repeated, involving all possible side effects. Operation should never be started
    * without the callback being passed (i.e. there should be no observable side effects before a callback is passed).
    * Implementation of `Async` should also be prepared to accept a callback before the previous one was notified
    * about the result (i.e. it should support concurrent execution).
    */
  type Async[T] = Callback[T] => Unit

  /**
    * Typeclass which captures the fact that some effect type constructor represents asynchronous computation and
    * can be converted to [[RawRest.Async]].
    */
  @implicitNotFound("${F} is not a valid asynchronous effect, ToAsync instance is missing")
  trait ToAsync[F[_]] {
    def toAsync[A](fa: F[A]): Async[A]
  }

  /**
    * Typeclass which captures the fact that some effect type constructor represents asynchronous computation and
    * can be constructed from [[RawRest.Async]].
    */
  @implicitNotFound("${F} is not a valid asynchronous effect, FromAsync instance is missing")
  trait FromAsync[F[_]] {
    def fromAsync[A](async: Async[A]): F[A]
  }

  /**
    * Raw type of an operation that executes a [[RestRequest]]. The operation should be run every time the
    * resulting `Async` value is passed a callback. It should not be run before that. Each run may involve side
    * effects, network communication, etc. Runs may be concurrent.
    * Request handlers should never throw exceptions but rather convert them into failing implementation of
    * `Async`. One way to do this is by wrapping the handler with [[safeHandle]].
    */
  type HandleRequest = RestRequest => Async[RestResponse]

  /**
    * Similar to [[HandleRequest]] but accepts already resolved path as a second argument.
    */
  type HandleResolvedRequest = (RestRequest, ResolvedCall) => Async[RestResponse]

  /**
    * Ensures that all possible exceptions thrown by a request handler are not propagated but converted into
    * an instance of `Async` that notifies its callbacks about the failure.
    */
  def safeHandle(handleRequest: HandleRequest): HandleRequest =
    request => safeAsync(handleRequest(request))

  private def guardedAsync[T](async: Async[T]): Async[T] = callback => {
    val called = new AtomicBoolean
    val guardedCallback: Callback[T] = result =>
      if (!called.getAndSet(true)) {
        callback(result) // may possibly throw but better let it fly rather than catch and ignore
      }
    try async(guardedCallback) catch {
      case NonFatal(t) =>
        // if callback was already called then we can't do much with the failure, rethrow it
        if (!called.getAndSet(true)) callback(Failure(t)) else throw t
    }
  }

  def safeAsync[T](async: => Async[T]): Async[T] =
    try guardedAsync(async) catch {
      case NonFatal(t) => failingAsync(t)
    }

  def readyAsync[T](result: Try[T]): Async[T] =
    callback => callback(result)

  def successfulAsync[T](value: T): Async[T] =
    readyAsync(Success(value))

  def failingAsync[T](cause: Throwable): Async[T] =
    readyAsync(Failure(cause))

  def mapAsync[A, B](async: Async[A])(f: A => B): Async[B] =
    cb => async(contramapCallback(cb)(f))

  def contramapCallback[A, B](callback: Callback[B])(f: A => B): Callback[A] =
    ta => callback(ta.map(f))

  def fromHandleRequest[Real: AsRealRpc : RestMetadata](handleRequest: HandleRequest): Real =
    RawRest.asReal(new DefaultRawRest(RestMetadata[Real], RestParameters.Empty, handleRequest))

  def asHandleRequest[Real: AsRawRpc : RestMetadata](real: Real): HandleRequest =
    RawRest.asRaw(real).asHandleRequest(RestMetadata[Real])

  def resolveAndHandle(metadata: RestMetadata[_])(handleResolved: HandleResolvedRequest): HandleRequest = {
    metadata.ensureValid()

    RawRest.safeHandle { request =>
      val path = request.parameters.path
      metadata.resolvePath(path) match {
        case Nil =>
          val message = s"path ${PathValue.encodeJoin(path)} not found"
          RawRest.successfulAsync(RestResponse.plain(404, message))
        case calls => request.method match {
          case HttpMethod.OPTIONS =>
            val meths = calls.iterator.map(_.method).flatMap {
              case HttpMethod.GET => List(HttpMethod.GET, HttpMethod.HEAD)
              case m => List(m)
            } ++ Iterator(HttpMethod.OPTIONS)
            val response = RestResponse(200, Mapping("Allow" -> HeaderValue(meths.mkString(","))), HttpBody.Empty)
            RawRest.successfulAsync(response)
          case wireMethod =>
            val head = wireMethod == HttpMethod.HEAD
            val req = if (head) request.copy(method = HttpMethod.GET) else request
            calls.find(_.method == req.method) match {
              case Some(call) =>
                val resp = handleResolved(req, call)
                if (head) RawRest.mapAsync(resp)(_.copy(body = HttpBody.empty)) else resp
              case None =>
                val message = s"$wireMethod not allowed on path ${PathValue.encodeJoin(path)}"
                RawRest.successfulAsync(RestResponse.plain(405, message))
            }
        }
      }
    }
  }

  private final class DefaultRawRest(metadata: RestMetadata[_], prefixHeaders: RestParameters, handleRequest: HandleRequest)
    extends RawRest {

    def prefix(name: String, parameters: RestParameters): Try[RawRest] =
      metadata.prefixMethods.get(name).map { prefixMeta =>
        val newHeaders = prefixHeaders.append(prefixMeta, parameters)
        Success(new DefaultRawRest(prefixMeta.result.value, newHeaders, handleRequest))
      } getOrElse Failure(new RestException(s"no such prefix method: $name"))

    def get(name: String, parameters: RestParameters): Async[RestResponse] =
      handleCustom(name, parameters, HttpBody.Empty)

    def handleJson(name: String, parameters: RestParameters, body: Mapping[JsonValue]): Async[RestResponse] =
      handleCustom(name, parameters, HttpBody.createJsonBody(body))

    def handleForm(name: String, parameters: RestParameters, body: Mapping[QueryValue]): Async[RestResponse] =
      handleCustom(name, parameters, HttpBody.createFormBody(body))

    def handleCustom(name: String, parameters: RestParameters, body: HttpBody): Async[RestResponse] =
      metadata.httpMethods.get(name).map { methodMeta =>
        val newHeaders = prefixHeaders.append(methodMeta, parameters)
        handleRequest(RestRequest(methodMeta.method, newHeaders, body))
      } getOrElse RawRest.failingAsync(new RestException(s"no such HTTP method: $name"))
  }
}

class RestException(msg: String, cause: Throwable = null) extends InvalidRpcCall(msg, cause)
