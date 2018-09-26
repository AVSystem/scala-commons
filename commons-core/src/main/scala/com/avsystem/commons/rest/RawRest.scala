package com.avsystem.commons
package rest

import java.util.concurrent.atomic.AtomicBoolean

import com.avsystem.commons.meta._
import com.avsystem.commons.rpc._

case class RestMethodCall(rpcName: String, pathParams: List[PathValue], metadata: RestMethodMetadata[_])
case class ResolvedPath(prefixes: List[RestMethodCall], finalCall: RestMethodCall, finalMetadata: HttpMethodMetadata[_]) {
  lazy val pathPattern: List[PathPatternElement] =
    if (prefixes.isEmpty) finalMetadata.pathPattern
    else (prefixes.iterator.flatMap(_.metadata.pathPattern.iterator) ++
      finalMetadata.pathPattern.iterator).toList

  def prepend(rpcName: String, pathParams: List[PathValue], metadata: PrefixMetadata[_]): ResolvedPath =
    copy(prefixes = RestMethodCall(rpcName, pathParams, metadata) :: prefixes)

  def rpcChainRepr: String =
    prefixes.iterator.map(_.rpcName).mkString("", "->", s"->${finalCall.rpcName}")
}

@methodTag[RestMethodTag]
trait RawRest {

  import RawRest._

  // declaration order of raw methods matters - it determines their priority!

  @multi @tried
  @tagged[Prefix](whenUntagged = new Prefix)
  @paramTag[RestParamTag](defaultTag = new Path)
  def prefix(@methodName name: String, @composite parameters: RestParameters): Try[RawRest]

  @multi @tried
  @tagged[GET]
  @paramTag[RestParamTag](defaultTag = new Query)
  def get(@methodName name: String, @composite parameters: RestParameters): Async[RestResponse]

  @multi @tried @annotated[FormBody]
  @tagged[BodyMethodTag](whenUntagged = new POST)
  @paramTag[RestParamTag](defaultTag = new BodyField)
  def handleForm(@methodName name: String, @composite parameters: RestParameters,
    @multi @tagged[BodyField] body: Mapping[QueryValue]): Async[RestResponse]

  @multi @tried
  @tagged[BodyMethodTag](whenUntagged = new POST)
  @paramTag[RestParamTag](defaultTag = new BodyField)
  def handle(@methodName name: String, @composite parameters: RestParameters,
    @multi @tagged[BodyField] body: Mapping[JsonValue]): Async[RestResponse]

  @multi @tried
  @tagged[BodyMethodTag](whenUntagged = new POST)
  @paramTag[RestParamTag]
  def handleSingle(@methodName name: String, @composite parameters: RestParameters,
    @encoded @tagged[Body] body: HttpBody): Async[RestResponse]

  def asHandleRequest(metadata: RestMetadata[_]): HandleRequest =
    RawRest.resolveAndHandle(metadata)(handleResolved)

  def handleResolved(request: RestRequest, resolved: ResolvedPath): Async[RestResponse] = {
    val RestRequest(method, parameters, body) = request
    val ResolvedPath(prefixes, finalCall, finalMetadata) = resolved
    val RestMethodCall(finalRpcName, finalPathParams, _) = finalCall

    def resolveCall(rawRest: RawRest, prefixes: List[RestMethodCall]): Async[RestResponse] = prefixes match {
      case RestMethodCall(rpcName, pathParams, _) :: tail =>
        rawRest.prefix(rpcName, parameters.copy(path = pathParams)) match {
          case Success(nextRawRest) => resolveCall(nextRawRest, tail)
          case Failure(e: HttpErrorException) => RawRest.successfulAsync(e.toResponse)
          case Failure(cause) => RawRest.failingAsync(cause)
        }
      case Nil =>
        val finalParameters = parameters.copy(path = finalPathParams)
        if (method == HttpMethod.GET)
          rawRest.get(finalRpcName, finalParameters)
        else if (finalMetadata.singleBody)
          rawRest.handleSingle(finalRpcName, finalParameters, body)
        else if (finalMetadata.formBody)
          rawRest.handleForm(finalRpcName, finalParameters, HttpBody.parseFormBody(body))
        else
          rawRest.handle(finalRpcName, finalParameters, HttpBody.parseJsonBody(body))
    }
    resolveCall(this, prefixes)
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
  type HandleResolvedRequest = (RestRequest, ResolvedPath) => Async[RestResponse]

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

  def fromHandleRequest[Real: AsRealRpc : RestMetadata](handleRequest: HandleRequest): Real =
    RawRest.asReal(new DefaultRawRest(RestMetadata[Real], RestParameters.Empty, handleRequest))

  def asHandleRequest[Real: AsRawRpc : RestMetadata](real: Real): HandleRequest =
    RawRest.asRaw(real).asHandleRequest(RestMetadata[Real])

  def resolveAndHandle(metadata: RestMetadata[_])(handleResolved: HandleResolvedRequest): HandleRequest = {
    metadata.ensureValid()
    RawRest.safeHandle { request =>
      metadata.resolvePath(request.method, request.parameters.path) match {
        case Right(resolved) => handleResolved(request, resolved)
        case Left(error) => RawRest.successfulAsync(error.toResponse)
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
      handleSingle(name, parameters, HttpBody.Empty)

    def handle(name: String, parameters: RestParameters, body: Mapping[JsonValue]): Async[RestResponse] =
      handleSingle(name, parameters, HttpBody.createJsonBody(body))

    def handleForm(name: String, parameters: RestParameters, body: Mapping[QueryValue]): Async[RestResponse] =
      handleSingle(name, parameters, HttpBody.createFormBody(body))

    def handleSingle(name: String, parameters: RestParameters, body: HttpBody): Async[RestResponse] =
      metadata.httpMethods.get(name).map { methodMeta =>
        val newHeaders = prefixHeaders.append(methodMeta, parameters)
        handleRequest(RestRequest(methodMeta.method, newHeaders, body))
      } getOrElse RawRest.failingAsync(new RestException(s"no such HTTP method: $name"))
  }
}

class RestException(msg: String, cause: Throwable = null) extends RpcException(msg, cause)
