package com.avsystem.commons
package rest

import com.avsystem.commons.annotation.{AnnotationAggregate, defaultsToName}
import com.avsystem.commons.misc.{AbstractValueEnum, AbstractValueEnumCompanion, EnumCtx}
import com.avsystem.commons.rpc._
import com.avsystem.commons.serialization.GenCodec.ReadFailure
import com.avsystem.commons.serialization.json.{JsonReader, JsonStringInput, JsonStringOutput}
import com.avsystem.commons.serialization.{GenCodec, GenKeyCodec}

/**
  * Base trait for tag annotations that determine how a REST method is translated into actual HTTP request.
  * A REST method may be annotated with one of HTTP method tags ([[GET]], [[PUT]], [[POST]], [[PATCH]], [[DELETE]])
  * which means that this method represents actual HTTP call and is expected to return a `Future[Result]` where
  * `Result` is encodable as [[RestResponse]].
  *
  * If a REST method is not annotated with any of HTTP method tags, [[Prefix]] is assumed by default which means
  * that this method only contributes to URL path, HTTP headers and query parameters but does not yet represent an
  * actual HTTP request. Instead, it is expected to return some other REST API trait.
  */
sealed trait RestMethodTag extends RpcTag {
  /**
    * HTTP URL path segment associated with REST method annotated with this tag. This path may be multipart
    * (i.e. contain slashes). It may also be empty which means that this particular REST method does not contribute
    * anything to URL path. If path is not specified explicitly, method name is used (the actual method name, not
    * `rpcName`).
    *
    * @example
    * {{{
    *   trait SomeRestApi {
    *     @GET("users/find")
    *     def findUser(userId: String): Future[User]
    *   }
    *   object SomeRestApi extends RestApiCompanion[SomeRestApi]
    * }}}
    */
  @defaultsToName def path: String
}

sealed abstract class HttpMethodTag(val method: HttpMethod) extends RestMethodTag with AnnotationAggregate

/**
  * Base trait for annotations representing HTTP methods which may define a HTTP body. This includes
  * [[PUT]], [[POST]], [[PATCH]] and [[DELETE]]. Parameters of REST methods annotated with one of these tags are
  * by default serialized into JSON (through encoding to [[JsonValue]]) and combined into JSON object that is sent
  * as HTTP body.
  *
  * Parameters may also contribute to URL path, HTTP headers and query parameters if annotated as
  * [[Path]], [[Header]] or [[Query]].
  *
  * REST method may also take a single parameter representing the entire HTTP body. Such parameter must be annotated
  * as [[Body]] and must be the only body parameter of that method. Value of this parameter will be encoded as
  * [[HttpBody]] which doesn't necessarily have to be JSON (it may define its own MIME type).
  *
  * @example
  * {{{
  *   trait SomeRestApi {
  *     @POST("users/create") def createUser(@Body user: User): Future[Unit]
  *     @PATCH("users/update") def updateUser(id: String, name: String): Future[User]
  *   }
  *   object SomeRestApi extends RestApiCompanion[SomeRestApi]
  * }}}
  */
sealed abstract class BodyMethodTag(method: HttpMethod) extends HttpMethodTag(method)

/**
  * REST method annotated as `@GET` will translate to HTTP GET request. By default, parameters of such method
  * are translated into URL query parameters (encoded as [[QueryValue]]). Alternatively, each parameter
  * may be annotated as [[Path]] or [[Header]] which means that it will be translated into HTTP header value
  *
  * @param path see [[RestMethodTag.path]]
  */
final class GET(val path: String = null) extends HttpMethodTag(HttpMethod.GET) {
  @rpcNamePrefix("GET_") type Implied
}

/** See [[BodyMethodTag]] */
final class POST(val path: String = null) extends BodyMethodTag(HttpMethod.POST) {
  @rpcNamePrefix("POST_") type Implied
}
/** See [[BodyMethodTag]] */
final class PATCH(val path: String = null) extends BodyMethodTag(HttpMethod.PATCH) {
  @rpcNamePrefix("PATCH_") type Implied
}
/** See [[BodyMethodTag]] */
final class PUT(val path: String = null) extends BodyMethodTag(HttpMethod.PUT) {
  @rpcNamePrefix("PUT_") type Implied
}
/** See [[BodyMethodTag]] */
final class DELETE(val path: String = null) extends BodyMethodTag(HttpMethod.DELETE) {
  @rpcNamePrefix("DELETE_") type Implied
}

/**
  * REST methods annotated as [[Prefix]] are expected to return another REST API trait as their result.
  * They do not yet represent an actual HTTP request but contribute to URL path, HTTP headers and query parameters.
  *
  * By default, parameters of a prefix method are interpreted as URL path fragments. Their values are encoded as
  * [[PathValue]] and appended to URL path. Alternatively, each parameter may also be explicitly annotated as
  * [[Header]] or [[Query]].
  *
  * NOTE: REST method is interpreted as prefix method by default which means that there is no need to apply [[Prefix]]
  * annotation explicitly unless you want to specify a custom path.
  *
  * @param path see [[RestMethodTag.path]]
  */
final class Prefix(val path: String = null) extends RestMethodTag

sealed trait RestParamTag extends RpcTag

/**
  * REST method parameters annotated as [[Path]] will be encoded as [[PathValue]] and appended to URL path, in the
  * declaration order. Parameters of [[Prefix]] REST methods are interpreted as [[Path]] parameters by default.
  */
final class Path(val pathSuffix: String = "") extends RestParamTag

/**
  * REST method parameters annotated as [[Header]] will be encoded as [[HeaderValue]] and added to HTTP headers.
  * Header name must be explicitly given as argument of this annotation.
  */
final class Header(override val name: String)
  extends rpcName(name) with RestParamTag

/**
  * REST method parameters annotated as [[Query]] will be encoded as [[QueryValue]] and added to URL query
  * parameters. Parameters of [[GET]] REST methods are interpreted as [[Query]] parameters by default.
  */
final class Query(@defaultsToName override val name: String = null)
  extends rpcName(name) with RestParamTag

sealed trait BodyTag extends RestParamTag

/**
  * REST method parameters annotated as [[JsonBodyParam]] will be encoded as [[JsonValue]] and combined into
  * a JSON object that will be sent as HTTP body. Body parameters are allowed only in REST methods annotated as
  * [[POST]], [[PATCH]], [[PUT]] or [[DELETE]]. Actually, parameters of these methods are interpreted as
  * [[JsonBodyParam]] by default which means that this annotation rarely needs to be applied explicitly.
  */
final class JsonBodyParam(@defaultsToName override val name: String = null)
  extends rpcName(name) with BodyTag

/**
  * REST methods that can send HTTP body ([[POST]], [[PATCH]], [[PUT]] and [[DELETE]]) may take a single
  * parameter annotated as [[Body]] which will be encoded as [[HttpBody]] and sent as the body of HTTP request.
  * Such a method may not define any other body parameters (although it may take additional [[Path]], [[Header]]
  * or [[Query]] parameters).
  *
  * The single body parameter may have a completely custom encoding to [[HttpBody]] which may define its own MIME type
  * and doesn't necessarily have to be JSON.
  */
final class Body extends BodyTag

sealed trait RestValue extends Any {
  def value: String
}

/**
  * Value used as encoding of [[Path]] parameters. Types that have `GenKeyCodec` instance have automatic encoding
  * to [[PathValue]].
  */
case class PathValue(value: String) extends AnyVal with RestValue
object PathValue {
  def split(path: String): List[PathValue] =
    path.split("/").iterator.filter(_.nonEmpty).map(PathValue(_)).toList
}

/**
  * Value used as encoding of [[Header]] parameters. Types that have `GenKeyCodec` instance have automatic encoding
  * to [[HeaderValue]].
  */
case class HeaderValue(value: String) extends AnyVal with RestValue

/**
  * Value used as encoding of [[Query]] parameters. Types that have `GenKeyCodec` instance have automatic encoding
  * to [[QueryValue]].
  */
case class QueryValue(value: String) extends AnyVal with RestValue

/**
  * Value used as encoding of [[JsonBodyParam]] parameters. Types that have `GenCodec` instance have automatic encoding
  * to [[JsonValue]].
  */
case class JsonValue(value: String) extends AnyVal with RestValue

object RestValue {
  implicit def pathValueDefaultAsRealRaw[T: GenKeyCodec]: AsRawReal[PathValue, T] =
    AsRawReal.create(v => PathValue(GenKeyCodec.write[T](v)), v => GenKeyCodec.read[T](v.value))
  implicit def headerValueDefaultAsRealRaw[T: GenKeyCodec]: AsRawReal[HeaderValue, T] =
    AsRawReal.create(v => HeaderValue(GenKeyCodec.write[T](v)), v => GenKeyCodec.read[T](v.value))
  implicit def queryValueDefaultAsRealRaw[T: GenKeyCodec]: AsRawReal[QueryValue, T] =
    AsRawReal.create(v => QueryValue(GenKeyCodec.write[T](v)), v => GenKeyCodec.read[T](v.value))
  implicit def jsonValueDefaultAsRealRaw[T: GenCodec]: AsRawReal[JsonValue, T] =
    AsRawReal.create(v => JsonValue(JsonStringOutput.write[T](v)), v => JsonStringInput.read[T](v.value))
}

/**
  * Value used to represent HTTP body. Also used as direct encoding of [[Body]] parameters. Types that have
  * encoding to [[JsonValue]] (e.g. types that have `GenCodec` instance) automatically have encoding to [[HttpBody]]
  * which uses application/json MIME type. There is also a specialized encoding provided for `Unit` which maps it
  * to empty HTTP body instead of JSON containing "null".
  *
  * @param content  raw HTTP body content
  * @param mimeType MIME type, i.e. HTTP `Content-Type` without charset specified
  */
case class HttpBody(content: String, mimeType: String) {
  def jsonValue: JsonValue = mimeType match {
    case HttpBody.JsonType => JsonValue(content)
    case _ => throw new ReadFailure(s"Expected application/json type, got $mimeType")
  }
}
object HttpBody {
  final val PlainType = "text/plain"
  final val JsonType = "application/json"

  def plain(value: String): HttpBody = HttpBody(value, PlainType)
  def json(json: JsonValue): HttpBody = HttpBody(json.value, JsonType)

  final val Empty: HttpBody = HttpBody.plain("")

  def createJsonBody(fields: NamedParams[JsonValue]): HttpBody =
    if (fields.isEmpty) HttpBody.Empty else {
      val sb = new JStringBuilder
      val oo = new JsonStringOutput(sb).writeObject()
      fields.foreach {
        case (key, JsonValue(json)) =>
          oo.writeField(key).writeRawJson(json)
      }
      oo.finish()
      HttpBody.json(JsonValue(sb.toString))
    }

  def parseJsonBody(body: HttpBody): NamedParams[JsonValue] =
    if (body.content.isEmpty) NamedParams.empty else {
      val oi = new JsonStringInput(new JsonReader(body.jsonValue.value)).readObject()
      val builder = NamedParams.newBuilder[JsonValue]
      while (oi.hasNext) {
        val fi = oi.nextField()
        builder += ((fi.fieldName, JsonValue(fi.readRawJson())))
      }
      builder.result()
    }

  implicit val emptyBodyForUnit: AsRawReal[HttpBody, Unit] =
    AsRawReal.create(_ => HttpBody.Empty, _ => ())
  implicit def httpBodyJsonAsRaw[T](implicit jsonAsRaw: AsRaw[JsonValue, T]): AsRaw[HttpBody, T] =
    AsRaw.create(v => HttpBody.json(jsonAsRaw.asRaw(v)))
  implicit def httpBodyJsonAsReal[T](implicit jsonAsReal: AsReal[JsonValue, T]): AsReal[HttpBody, T] =
    AsReal.create(v => jsonAsReal.asReal(v.jsonValue))
}

/**
  * Enum representing HTTP methods.
  */
final class HttpMethod(implicit enumCtx: EnumCtx) extends AbstractValueEnum
object HttpMethod extends AbstractValueEnumCompanion[HttpMethod] {
  final val GET, PUT, POST, PATCH, DELETE: Value = new HttpMethod
}

case class RestHeaders(
  @multi @tagged[Path] path: List[PathValue],
  @multi @tagged[Header] headers: NamedParams[HeaderValue],
  @multi @tagged[Query] query: NamedParams[QueryValue]
) {
  def append(method: RestMethodMetadata[_], otherHeaders: RestHeaders): RestHeaders = RestHeaders(
    path ::: method.applyPathParams(otherHeaders.path),
    headers ++ otherHeaders.headers,
    query ++ otherHeaders.query
  )
}
object RestHeaders {
  final val Empty = RestHeaders(Nil, NamedParams.empty, NamedParams.empty)
}

class HttpErrorException(code: Int, payload: String)
  extends RuntimeException(s"$code: $payload")

case class RestRequest(method: HttpMethod, headers: RestHeaders, body: HttpBody)
case class RestResponse(code: Int, body: HttpBody)
object RestResponse {
  implicit def defaultFutureAsRaw[T](implicit bodyAsRaw: AsRaw[HttpBody, T]): AsRaw[Future[RestResponse], Future[T]] =
    AsRaw.create(_.mapNow(v => RestResponse(200, bodyAsRaw.asRaw(v))))
  implicit def defaultFutureAsReal[T](implicit bodyAsReal: AsReal[HttpBody, T]): AsReal[Future[RestResponse], Future[T]] =
    AsReal.create(_.mapNow {
      case RestResponse(200, body) => bodyAsReal.asReal(body)
      case RestResponse(code, body) => throw new HttpErrorException(code, body.content)
    })
}

@methodTag[RestMethodTag, Prefix]
@paramTag[RestParamTag, RestParamTag]
trait RawRest {
  @multi
  @tagged[Prefix]
  @paramTag[RestParamTag, Path]
  def prefix(@methodName name: String, @composite headers: RestHeaders): RawRest

  @multi
  @tagged[GET]
  @paramTag[RestParamTag, Query]
  def get(@methodName name: String, @composite headers: RestHeaders): Future[RestResponse]

  @multi
  @tagged[BodyMethodTag]
  @paramTag[RestParamTag, JsonBodyParam]
  def handle(@methodName name: String, @composite headers: RestHeaders,
    @multi @tagged[JsonBodyParam] body: NamedParams[JsonValue]): Future[RestResponse]

  @multi
  @tagged[BodyMethodTag]
  def handleSingle(@methodName name: String, @composite headers: RestHeaders,
    @encoded @tagged[Body] body: HttpBody): Future[RestResponse]

  def asHandleRequest(metadata: RestMetadata[_]): RestRequest => Future[RestResponse] = {
    metadata.ensureUnambiguousPaths()
    locally[RestRequest => Future[RestResponse]] {
      case RestRequest(method, headers, body) => metadata.resolvePath(method, headers.path).toList match {
        case List(ResolvedPath(prefixes, RpcWithPath(finalRpcName, finalPathParams), singleBody)) =>
          val finalRawRest = prefixes.foldLeft(this) {
            case (rawRest, RpcWithPath(rpcName, pathParams)) =>
              rawRest.prefix(rpcName, headers.copy(path = pathParams))
          }
          val finalHeaders = headers.copy(path = finalPathParams)

          if (method == HttpMethod.GET) finalRawRest.get(finalRpcName, finalHeaders)
          else if (singleBody) finalRawRest.handleSingle(finalRpcName, finalHeaders, body)
          else finalRawRest.handle(finalRpcName, finalHeaders, HttpBody.parseJsonBody(body))

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
  def fromHandleRequest[Real: AsRealRpc : RestMetadata](handleRequest: RestRequest => Future[RestResponse]): Real =
    RawRest.asReal(new DefaultRawRest(RestMetadata[Real], RestHeaders.Empty, handleRequest))

  def asHandleRequest[Real: AsRawRpc : RestMetadata](real: Real): RestRequest => Future[RestResponse] =
    RawRest.asRaw(real).asHandleRequest(RestMetadata[Real])

  private final class DefaultRawRest(
    metadata: RestMetadata[_],
    prefixHeaders: RestHeaders,
    handleRequest: RestRequest => Future[RestResponse]
  ) extends RawRest {

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

  trait ClientMacroInstances[Real] {
    def metadata: RestMetadata[Real]
    def asReal: AsRealRpc[Real]
  }

  trait ServerMacroInstances[Real] {
    def metadata: RestMetadata[Real]
    def asRaw: AsRawRpc[Real]
  }

  trait FullMacroInstances[Real] extends ClientMacroInstances[Real] with ServerMacroInstances[Real]

  implicit def clientInstances[Real]: ClientMacroInstances[Real] = macro macros.rest.RestMacros.instances[Real]
  implicit def serverInstances[Real]: ServerMacroInstances[Real] = macro macros.rest.RestMacros.instances[Real]
  implicit def fullInstances[Real]: FullMacroInstances[Real] = macro macros.rest.RestMacros.instances[Real]
}

/**
  * Base class for companions of REST API traits used only for REST clients to external services.
  */
abstract class RestClientApiCompanion[Real](implicit instances: RawRest.ClientMacroInstances[Real]) {
  implicit def restMetadata: RestMetadata[Real] = instances.metadata
  implicit def rawRestAsReal: RawRest.AsRealRpc[Real] = instances.asReal
}

/**
  * Base class for companions of REST API traits used only for REST servers exposed to external world.
  */
abstract class RestServerApiCompanion[Real](implicit instances: RawRest.ServerMacroInstances[Real]) {
  implicit def restMetadata: RestMetadata[Real] = instances.metadata
  implicit def realAsRawRest: RawRest.AsRawRpc[Real] = instances.asRaw
}

/**
  * Base class for companions of REST API traits used for both REST clients and servers.
  */
abstract class RestApiCompanion[Real](implicit instances: RawRest.FullMacroInstances[Real]) {
  implicit def restMetadata: RestMetadata[Real] = instances.metadata
  implicit def rawRestAsReal: RawRest.AsRealRpc[Real] = instances.asReal
  implicit def realAsRawRest: RawRest.AsRawRpc[Real] = instances.asRaw
}

case class RpcWithPath(rpcName: String, pathParams: List[PathValue])
case class ResolvedPath(prefixes: List[RpcWithPath], finalCall: RpcWithPath, singleBody: Boolean) {
  def prepend(rpcName: String, pathParams: List[PathValue]): ResolvedPath =
    copy(prefixes = RpcWithPath(rpcName, pathParams) :: prefixes)

  def rpcChainRepr: String =
    prefixes.iterator.map(_.rpcName).mkString("", "->", s"->${finalCall.rpcName}")
}

@methodTag[RestMethodTag, Prefix]
@paramTag[RestParamTag, JsonBodyParam]
case class RestMetadata[T](
  @multi @tagged[Prefix] prefixMethods: Map[String, PrefixMetadata[_]],
  @multi @tagged[HttpMethodTag] httpMethods: Map[String, HttpMethodMetadata[_]]
) {
  def ensureUnambiguousPaths(): Unit = {
    val trie = new RestMetadata.Trie
    trie.fillWith(this)
    trie.mergeWildcardToNamed()
    val ambiguities = new MListBuffer[(String, List[String])]
    trie.collectAmbiguousCalls(ambiguities)
    if (ambiguities.nonEmpty) {
      val problems = ambiguities.map { case (path, chains) =>
        s"$path may result from multiple calls:\n  ${chains.mkString("\n  ")}"
      }
      throw new IllegalArgumentException(s"REST API has ambiguous paths:\n${problems.mkString("\n")}")
    }
  }

  def resolvePath(method: HttpMethod, path: List[PathValue]): Iterator[ResolvedPath] = {
    val asFinalCall = for {
      (rpcName, m) <- httpMethods.iterator if m.method == method
      (pathParams, Nil) <- m.extractPathParams(path)
    } yield ResolvedPath(Nil, RpcWithPath(rpcName, pathParams), m.singleBody)

    val usingPrefix = for {
      (rpcName, prefix) <- prefixMethods.iterator
      (pathParams, pathTail) <- prefix.extractPathParams(path).iterator
      suffixPath <- prefix.result.value.resolvePath(method, pathTail)
    } yield suffixPath.prepend(rpcName, pathParams)

    asFinalCall ++ usingPrefix
  }
}
object RestMetadata extends RpcMetadataCompanion[RestMetadata] {
  private class Trie {
    val rpcChains: Map[HttpMethod, MBuffer[String]] =
      HttpMethod.values.mkMap(identity, _ => new MArrayBuffer[String])

    val byName: MMap[String, Trie] = new MHashMap
    var wildcard: Opt[Trie] = Opt.Empty

    def forPattern(pattern: List[Opt[PathValue]]): Trie = pattern match {
      case Nil => this
      case Opt(PathValue(pathName)) :: tail =>
        byName.getOrElseUpdate(pathName, new Trie).forPattern(tail)
      case Opt.Empty :: tail =>
        wildcard.getOrElse(new Trie().setup(t => wildcard = Opt(t))).forPattern(tail)
    }

    def fillWith(metadata: RestMetadata[_], prefixStack: List[(String, PrefixMetadata[_])] = Nil): Unit = {
      def prefixChain: String =
        prefixStack.reverseIterator.map({ case (k, _) => k }).mkStringOrEmpty("", "->", "->")

      metadata.prefixMethods.foreach { case entry@(rpcName, pm) =>
        if (prefixStack.contains(entry)) {
          throw new IllegalArgumentException(
            s"call chain $prefixChain$rpcName is recursive, recursively defined server APIs are forbidden")
        }
        forPattern(pm.pathPattern).fillWith(pm.result.value, entry :: prefixStack)
      }
      metadata.httpMethods.foreach { case (rpcName, hm) =>
        forPattern(hm.pathPattern).rpcChains(hm.method) += s"$prefixChain${rpcName.stripPrefix(s"${hm.method}_")}"
      }
    }

    private def merge(other: Trie): Unit = {
      HttpMethod.values.foreach { meth =>
        rpcChains(meth) ++= other.rpcChains(meth)
      }
      for (w <- wildcard; ow <- other.wildcard) w.merge(ow)
      wildcard = wildcard orElse other.wildcard
      other.byName.foreach { case (name, trie) =>
        byName.getOrElseUpdate(name, new Trie).merge(trie)
      }
    }

    def mergeWildcardToNamed(): Unit = wildcard.foreach { wc =>
      wc.mergeWildcardToNamed()
      byName.values.foreach { trie =>
        trie.merge(wc)
        trie.mergeWildcardToNamed()
      }
    }

    def collectAmbiguousCalls(ambiguities: MBuffer[(String, List[String])], pathPrefix: List[String] = Nil): Unit = {
      rpcChains.foreach { case (method, chains) =>
        if (chains.size > 1) {
          val path = pathPrefix.reverse.mkString(s"$method /", "/", "")
          ambiguities += ((path, chains.toList))
        }
      }
      wildcard.foreach(_.collectAmbiguousCalls(ambiguities, "*" :: pathPrefix))
      byName.foreach { case (name, trie) =>
        trie.collectAmbiguousCalls(ambiguities, name :: pathPrefix)
      }
    }
  }
}

sealed abstract class RestMethodMetadata[T] extends TypedMetadata[T] {
  def methodPath: List[PathValue]
  def headersMetadata: RestHeadersMetadata

  val pathPattern: List[Opt[PathValue]] =
    methodPath.map(Opt(_)) ++ headersMetadata.path.flatMap(pp => Opt.Empty :: pp.pathSuffix.map(Opt(_)))

  def applyPathParams(params: List[PathValue]): List[PathValue] = {
    def loop(params: List[PathValue], pattern: List[Opt[PathValue]]): List[PathValue] =
      (params, pattern) match {
        case (Nil, Nil) => Nil
        case (_, Opt(patternHead) :: patternTail) => patternHead :: loop(params, patternTail)
        case (param :: paramsTail, Opt.Empty :: patternTail) => param :: loop(paramsTail, patternTail)
        case _ => throw new IllegalArgumentException(
          s"got ${params.size} path params, expected ${headersMetadata.path.size}")
      }
    loop(params, pathPattern)
  }

  def extractPathParams(path: List[PathValue]): Opt[(List[PathValue], List[PathValue])] = {
    def loop(path: List[PathValue], pattern: List[Opt[PathValue]]): Opt[(List[PathValue], List[PathValue])] =
      (path, pattern) match {
        case (pathTail, Nil) => Opt((Nil, pathTail))
        case (param :: pathTail, Opt.Empty :: patternTail) =>
          loop(pathTail, patternTail).map { case (params, tail) => (param :: params, tail) }
        case (pathHead :: pathTail, Opt(patternHead) :: patternTail) if pathHead == patternHead =>
          loop(pathTail, patternTail)
        case _ => Opt.Empty
      }
    loop(path, pathPattern)
  }
}

@paramTag[RestParamTag, Path]
case class PrefixMetadata[T](
  @reifyName name: String,
  @optional @reifyAnnot methodTag: Opt[Prefix],
  @composite headersMetadata: RestHeadersMetadata,
  @checked @infer result: RestMetadata.Lazy[T]
) extends RestMethodMetadata[T] {
  def methodPath: List[PathValue] =
    PathValue.split(methodTag.map(_.path).getOrElse(name))
}

case class HttpMethodMetadata[T](
  @reifyAnnot methodTag: HttpMethodTag,
  @composite headersMetadata: RestHeadersMetadata,
  @multi @tagged[BodyTag] bodyParams: Map[String, BodyParamMetadata[_]]
) extends RestMethodMetadata[Future[T]] {
  val method: HttpMethod = methodTag.method
  val singleBody: Boolean = bodyParams.values.exists(_.singleBody)
  def methodPath: List[PathValue] = PathValue.split(methodTag.path)
}

case class RestHeadersMetadata(
  @multi @tagged[Path] path: List[PathParamMetadata[_]],
  @multi @tagged[Header] headers: Map[String, HeaderParamMetadata[_]],
  @multi @tagged[Query] query: Map[String, QueryParamMetadata[_]]
)

case class PathParamMetadata[T](@optional @reifyAnnot pathAnnot: Opt[Path]) extends TypedMetadata[T] {
  val pathSuffix: List[PathValue] = PathValue.split(pathAnnot.fold("")(_.pathSuffix))
}

case class HeaderParamMetadata[T]() extends TypedMetadata[T]
case class QueryParamMetadata[T]() extends TypedMetadata[T]
case class BodyParamMetadata[T](@hasAnnot[Body] singleBody: Boolean) extends TypedMetadata[T]
