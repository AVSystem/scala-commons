package com.avsystem.commons
package rest

import com.avsystem.commons.rpc._

case class RpcWithPath(rpcName: String, pathParams: List[PathValue])
case class ResolvedPath(prefixes: List[RpcWithPath], finalCall: RpcWithPath, singleBody: Boolean) {
  def prepend(rpcName: String, pathParams: List[PathValue]): ResolvedPath =
    copy(prefixes = RpcWithPath(rpcName, pathParams) :: prefixes)

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
    locally[RawRest.HandleRequest] {
      case RestRequest(method, headers, body) => metadata.resolvePath(method, headers.path).toList match {
        case List(ResolvedPath(prefixes, RpcWithPath(finalRpcName, finalPathParams), singleBody)) =>
          val finalRawRest = prefixes.foldLeft(this) {
            case (rawRest, RpcWithPath(rpcName, pathParams)) =>
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
          throw new IllegalArgumentException(s"path $pathStr is ambiguous, it could map to following calls:$callsRepr")
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
        throw new IllegalArgumentException(s"no such prefix method: $name"))
      val newHeaders = prefixHeaders.append(prefixMeta, headers)
      new DefaultRawRest(prefixMeta.result.value, newHeaders, handleRequest)
    }

    def get(name: String, headers: RestHeaders): Future[RestResponse] =
      handleSingle(name, headers, HttpBody.Empty)

    def handle(name: String, headers: RestHeaders, body: NamedParams[JsonValue]): Future[RestResponse] = {
      val methodMeta = metadata.httpMethods.getOrElse(name,
        throw new IllegalArgumentException(s"no such HTTP method: $name"))
      val newHeaders = prefixHeaders.append(methodMeta, headers)
      handleRequest(RestRequest(methodMeta.method, newHeaders, HttpBody.createJsonBody(body)))
    }

    def handleSingle(name: String, headers: RestHeaders, body: HttpBody): Future[RestResponse] = {
      val methodMeta = metadata.httpMethods.getOrElse(name,
        throw new IllegalArgumentException(s"no such HTTP method: $name"))
      val newHeaders = prefixHeaders.append(methodMeta, headers)
      handleRequest(RestRequest(methodMeta.method, newHeaders, body))
    }
  }

  trait ClientInstances[Real, I] extends RpcMacroInstances[Real] {
    type Implicits = I
    def metadata(implicits: I): RestMetadata[Real]
    def asReal(implicits: I): AsRealRpc[Real]
  }
  trait ServerInstances[Real, I] extends RpcMacroInstances[Real] {
    type Implicits = I
    def metadata(implicits: I): RestMetadata[Real]
    def asRaw(implicits: I): AsRawRpc[Real]
  }
  trait FullInstances[Real, I] extends RpcMacroInstances[Real] {
    type Implicits = I
    def metadata(implicits: I): RestMetadata[Real]
    def asRawReal(implicits: I): AsRawRealRpc[Real]
  }

  // I have no idea why I can't just create one macro in `RpcMacroInstances` companion to rule them all
  implicit def clientInstances[Real, I]: ClientInstances[Real, I] =
  macro macros.rpc.RpcMacros.macroInstances[ClientInstances[Real, I], Real]
  implicit def serverInstances[Real, I]: ServerInstances[Real, I] =
  macro macros.rpc.RpcMacros.macroInstances[ServerInstances[Real, I], Real]
  implicit def clientServerInstances[Real, I]: FullInstances[Real, I] =
  macro macros.rpc.RpcMacros.macroInstances[FullInstances[Real, I], Real]

  /** @see [[FullApiCompanion]]*/
  abstract class ClientApiCompanion[Real, I](implicit instances: ClientInstances[Real, I]) { this: I =>
    implicit final lazy val restMetadata: RestMetadata[Real] = instances.metadata(this)
    implicit final lazy val restAsReal: AsRealRpc[Real] = instances.asReal(this)
  }
  /** @see [[FullApiCompanion]]*/
  abstract class ServerApiCompanion[Real, I](implicit instances: ServerInstances[Real, I]) { this: I =>
    implicit final lazy val restMetadata: RestMetadata[Real] = instances.metadata(this)
    implicit final lazy val restAsRaw: AsRawRpc[Real] = instances.asRaw(this)
  }
  /**
    * Base class for REST trait companions. Reduces boilerplate needed in order to define appropriate instances
    * of `AsRawReal` and `RestMetadata` for given trait. The `I` type parameter lets you inject additional implicits
    * into macro materialization of these instances, e.g. [[DefaultRestImplicits]].
    * Usually, for even less boilerplate, this base class is extended by yet another abstract class which fixes
    * the `I` type, e.g. [[RestApiCompanion]].
    */
  abstract class FullApiCompanion[Real, I](implicit instances: FullInstances[Real, I]) { this: I =>
    implicit final lazy val restMetadata: RestMetadata[Real] = instances.metadata(this)
    implicit final lazy val restAsRawReal: AsRawRealRpc[Real] = instances.asRawReal(this)
  }
}
