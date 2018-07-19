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
  def get(@methodName name: String, @composite headers: RestHeaders): Future[RestResponse]

  @multi
  @tagged[BodyMethodTag](whenUntagged = new POST)
  @paramTag[RestParamTag](defaultTag = new JsonBodyParam)
  def handle(@methodName name: String, @composite headers: RestHeaders,
    @multi @tagged[JsonBodyParam] body: NamedParams[JsonValue]): Future[RestResponse]

  @multi
  @tagged[BodyMethodTag](whenUntagged = new POST)
  @paramTag[RestParamTag]
  def handleSingle(@methodName name: String, @composite headers: RestHeaders,
    @encoded @tagged[Body] body: HttpBody): Future[RestResponse]

  def asHandleRequest(metadata: RestMetadata[_]): RawRest.HandleRequest = {
    metadata.ensureUnambiguousPaths()
    metadata.ensureUniqueParams(Nil)
    locally[RawRest.HandleRequest] {
      case RestRequest(method, headers, body) => metadata.resolvePath(method, headers.path).toList match {
        case List(ResolvedPath(prefixes, RestMethodCall(finalRpcName, finalPathParams, _), singleBody)) =>
          val finalRawRest = prefixes.foldLeft(this) {
            case (rawRest, RestMethodCall(rpcName, pathParams, _)) =>
              rawRest.prefix(rpcName, headers.copy(path = pathParams))
          }
          val finalHeaders = headers.copy(path = finalPathParams)

          def result: Future[RestResponse] =
            if (method == HttpMethod.GET) finalRawRest.get(finalRpcName, finalHeaders)
            else if (singleBody) finalRawRest.handleSingle(finalRpcName, finalHeaders, body)
            else finalRawRest.handle(finalRpcName, finalHeaders, HttpBody.parseJsonBody(body))

          result.catchFailures

        case Nil =>
          val pathStr = headers.path.iterator.map(_.value).mkString("/")
          Future.successful(RestResponse(404, HttpBody.plain(s"path $pathStr not found")))

        case multiple =>
          val pathStr = headers.path.iterator.map(_.value).mkString("/")
          val callsRepr = multiple.iterator.map(p => s"  ${p.rpcChainRepr}").mkString("\n", "\n", "")
          throw new RestException(s"path $pathStr is ambiguous, it could map to following calls:$callsRepr")
      }
    }
  }
}

object RawRest extends RawRpcCompanion[RawRest] {
  type HandleRequest = RestRequest => Future[RestResponse]

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

    def get(name: String, headers: RestHeaders): Future[RestResponse] =
      handleSingle(name, headers, HttpBody.Empty)

    def handle(name: String, headers: RestHeaders, body: NamedParams[JsonValue]): Future[RestResponse] =
      handleSingle(name, headers, HttpBody.createJsonBody(body))

    def handleSingle(name: String, headers: RestHeaders, body: HttpBody): Future[RestResponse] = {
      val methodMeta = metadata.httpMethods.getOrElse(name,
        throw new RestException(s"no such HTTP method: $name"))
      val newHeaders = prefixHeaders.append(methodMeta, headers)
      handleRequest(RestRequest(methodMeta.method, newHeaders, body))
    }
  }
}

class RestException(msg: String, cause: Throwable = null) extends RpcException(msg, cause)
