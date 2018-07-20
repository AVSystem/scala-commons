package com.avsystem.commons
package rest

import com.avsystem.commons.rpc._

case class RestMethodCall(rpcName: String, pathParams: List[PathValue], metadata: RestMethodMetadata[_])
case class ResolvedPath(prefixes: List[RestMethodCall], finalCall: RestMethodCall, singleBody: Boolean) {
  def prepend(rpcName: String, pathParams: List[PathValue], metadata: PrefixMetadata[_]): ResolvedPath =
    copy(prefixes = RestMethodCall(rpcName, pathParams, metadata) :: prefixes)

  def rpcChainRepr: String =
    prefixes.iterator.map(_.rpcName).mkString("", "->", s"->${finalCall.rpcName}")
}

@methodTag[RestMethodTag]
trait RawRest {
  @multi
  @tagged[Prefix](whenUntagged = new Prefix)
  @paramTag[RestParamTag](defaultTag = new Path)
  def prefix(@methodName name: String, @composite headers: RestHeaders): RawRest

  @multi
  @tagged[GET]
  @paramTag[RestParamTag](defaultTag = new Query)
  def get(@methodName name: String, @composite headers: RestHeaders): RawRest.Async[RestResponse]

  @multi
  @tagged[BodyMethodTag](whenUntagged = new POST)
  @paramTag[RestParamTag](defaultTag = new JsonBodyParam)
  def handle(@methodName name: String, @composite headers: RestHeaders,
    @multi @tagged[JsonBodyParam] body: NamedParams[JsonValue]): RawRest.Async[RestResponse]

  @multi
  @tagged[BodyMethodTag](whenUntagged = new POST)
  @paramTag[RestParamTag]
  def handleSingle(@methodName name: String, @composite headers: RestHeaders,
    @encoded @tagged[Body] body: HttpBody): RawRest.Async[RestResponse]

  def asHandleRequest(metadata: RestMetadata[_]): RawRest.HandleRequest = {
    metadata.ensureUnambiguousPaths()
    metadata.ensureUniqueParams(Nil)
    RawRest.safeHandle { case RestRequest(method, headers, body) =>
      metadata.resolvePath(method, headers.path).toList match {
        case List(ResolvedPath(prefixes, RestMethodCall(finalRpcName, finalPathParams, _), singleBody)) =>
          val finalRawRest = prefixes.foldLeft(this) {
            case (rawRest, RestMethodCall(rpcName, pathParams, _)) =>
              rawRest.prefix(rpcName, headers.copy(path = pathParams))
          }
          val finalHeaders = headers.copy(path = finalPathParams)

          if (method == HttpMethod.GET) finalRawRest.get(finalRpcName, finalHeaders)
          else if (singleBody) finalRawRest.handleSingle(finalRpcName, finalHeaders, body)
          else finalRawRest.handle(finalRpcName, finalHeaders, HttpBody.parseJsonBody(body))

        case Nil =>
          val pathStr = headers.path.iterator.map(_.value).mkString("/")
          RawRest.successfulAsync(RestResponse(404, HttpBody.plain(s"path $pathStr not found")))

        case multiple =>
          val pathStr = headers.path.iterator.map(_.value).mkString("/")
          val callsRepr = multiple.iterator.map(p => s"  ${p.rpcChainRepr}").mkString("\n", "\n", "")
          throw new RestException(s"path $pathStr is ambiguous, it could map to following calls:$callsRepr")
      }
    }
  }
}

object RawRest extends RawRpcCompanion[RawRest] {
  /**
    * The most low-level realization of asynchronous computation:
    * something that accepts a callback on a value or failure.
    */
  type Callback[T] = Try[T] => Unit
  type Async[T] = Callback[T] => Unit
  type HandleRequest = RestRequest => Async[RestResponse]

  def safeHandle(handleRequest: HandleRequest): HandleRequest =
    request => try handleRequest(request) catch {
      case HttpErrorException(code, payload) =>
        successfulAsync(RestResponse(code, payload.fold(HttpBody.empty)(HttpBody.plain)))
      case NonFatal(t) =>
        failingAsync(t)
    }

  def safeAsync[T](async: => Async[T]): Async[T] =
    try async catch {
      case NonFatal(t) => failingAsync(t)
    }

  def successfulAsync[T](value: T): Async[T] =
    callback => callback(Success(value))

  def failingAsync[T](cause: Throwable): Async[T] =
    callback => callback(Failure(cause))

  def fromHandleRequest[Real: AsRealRpc : RestMetadata](handleRequest: HandleRequest): Real =
    RawRest.asReal(new DefaultRawRest(RestMetadata[Real], RestHeaders.Empty, handleRequest))

  def asHandleRequest[Real: AsRawRpc : RestMetadata](real: Real): HandleRequest =
    RawRest.asRaw(real).asHandleRequest(RestMetadata[Real])

  private final class DefaultRawRest(metadata: RestMetadata[_], prefixHeaders: RestHeaders, handleRequest: HandleRequest)
    extends RawRest {

    def prefix(name: String, headers: RestHeaders): RawRest = {
      val prefixMeta = metadata.prefixMethods.getOrElse(name,
        throw new RestException(s"no such prefix method: $name"))
      val newHeaders = prefixHeaders.append(prefixMeta, headers)
      new DefaultRawRest(prefixMeta.result.value, newHeaders, handleRequest)
    }

    def get(name: String, headers: RestHeaders): Async[RestResponse] =
      handleSingle(name, headers, HttpBody.Empty)

    def handle(name: String, headers: RestHeaders, body: NamedParams[JsonValue]): Async[RestResponse] =
      handleSingle(name, headers, HttpBody.createJsonBody(body))

    def handleSingle(name: String, headers: RestHeaders, body: HttpBody): Async[RestResponse] = {
      val methodMeta = metadata.httpMethods.getOrElse(name,
        throw new RestException(s"no such HTTP method: $name"))
      val newHeaders = prefixHeaders.append(methodMeta, headers)
      handleRequest(RestRequest(methodMeta.method, newHeaders, body))
    }
  }
}

class RestException(msg: String, cause: Throwable = null) extends RpcException(msg, cause)
