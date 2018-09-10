package com.avsystem.commons
package rest

import com.avsystem.commons.meta._
import com.avsystem.commons.rest.openapi._
import com.avsystem.commons.rpc._
import com.avsystem.commons.serialization.{transientDefault, whenAbsent}

import scala.annotation.implicitNotFound

@methodTag[RestMethodTag]
case class RestMetadata[T](
  @multi @tagged[Prefix](whenUntagged = new Prefix)
  @paramTag[RestParamTag](defaultTag = new Path)
  @rpcMethodMetadata prefixMethods: Mapping[PrefixMetadata[_]],

  @multi @tagged[GET]
  @paramTag[RestParamTag](defaultTag = new Query)
  @rpcMethodMetadata httpGetMethods: Mapping[HttpMethodMetadata[_]],

  @multi @tagged[BodyMethodTag](whenUntagged = new POST)
  @paramTag[RestParamTag](defaultTag = new JsonBodyParam)
  @rpcMethodMetadata httpBodyMethods: Mapping[HttpMethodMetadata[_]]
) {
  val httpMethods: Mapping[HttpMethodMetadata[_]] =
    httpGetMethods ++ httpBodyMethods

  def ensureUniqueParams(prefixes: List[(String, PrefixMetadata[_])]): Unit = {
    def ensureUniqueParams(methodName: String, method: RestMethodMetadata[_]): Unit = {
      for {
        (prefixName, prefix) <- prefixes
        headerParam <- method.parametersMetadata.headers.keys
        if prefix.parametersMetadata.headers.contains(headerParam)
      } throw new InvalidRestApiException(
        s"Header parameter $headerParam of $methodName collides with header parameter of the same name in prefix $prefixName")

      for {
        (prefixName, prefix) <- prefixes
        queryParam <- method.parametersMetadata.query.keys
        if prefix.parametersMetadata.query.contains(queryParam)
      } throw new InvalidRestApiException(
        s"Query parameter $queryParam of $methodName collides with query parameter of the same name in prefix $prefixName")
    }

    prefixMethods.foreach {
      case (name, prefix) =>
        ensureUniqueParams(name, prefix)
        prefix.result.value.ensureUniqueParams((name, prefix) :: prefixes)
    }
    (httpGetMethods ++ httpBodyMethods).foreach {
      case (name, method) => ensureUniqueParams(name, method)
    }
  }

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
      throw new InvalidRestApiException(s"REST API has ambiguous paths:\n${problems.mkString("\n")}")
    }
  }

  def resolvePath(method: HttpMethod, path: List[PathValue]): Opt[ResolvedPath] = {
    def resolve(method: HttpMethod, path: List[PathValue]): Iterator[ResolvedPath] = {
      val asFinalCall = for {
        (rpcName, m) <- httpMethods.iterator if m.method == method
        (pathParams, Nil) <- m.extractPathParams(path)
      } yield ResolvedPath(Nil, RestMethodCall(rpcName, pathParams, m), m.singleBody)

      val usingPrefix = for {
        (rpcName, prefix) <- prefixMethods.iterator
        (pathParams, pathTail) <- prefix.extractPathParams(path).iterator
        suffixPath <- prefix.result.value.resolvePath(method, pathTail)
      } yield suffixPath.prepend(rpcName, pathParams, prefix)

      asFinalCall ++ usingPrefix
    }
    resolve(method, path).toList match {
      case Nil => Opt.Empty
      case single :: Nil => Opt(single)
      case multiple =>
        val pathStr = path.iterator.map(_.value).mkString("/")
        val callsRepr = multiple.iterator.map(p => s"  ${p.rpcChainRepr}").mkString("\n", "\n", "")
        throw new RestException(s"path $pathStr is ambiguous, it could map to following calls:$callsRepr")
    }
  }

  def openapiOperations(resolver: SchemaResolver): Iterator[(String, HttpMethod, Operation)] =
    prefixMethods.valuesIterator.flatMap(_.openapiOperations(resolver)) ++
      httpMethods.valuesIterator.map(m => (m.openapiPath, m.method, m.openapiOperation(resolver)))

  def openapiPaths(resolver: SchemaResolver): Paths = {
    val pathsMap = new MLinkedHashMap[String, MLinkedHashMap[HttpMethod, Operation]]
    openapiOperations(resolver).foreach {
      case (path, httpMethod, operation) =>
        val opsMap = pathsMap.getOrElseUpdate(path, new MLinkedHashMap)
        opsMap(httpMethod) = operation
    }
    Paths(pathsMap.iterator.map { case (path, ops) =>
      val pathItem = PathItem(
        get = ops.getOpt(HttpMethod.GET).toOptArg,
        put = ops.getOpt(HttpMethod.PUT).toOptArg,
        post = ops.getOpt(HttpMethod.POST).toOptArg,
        patch = ops.getOpt(HttpMethod.PATCH).toOptArg,
        delete = ops.getOpt(HttpMethod.DELETE).toOptArg,
      )
      (path, RefOr(pathItem))
    }.toMap)
  }

  def openapi(info: Info): OpenApi = {
    val registry = new SchemaRegistry(n => s"#/components/schemas/$n")
    val paths = openapiPaths(registry)
    OpenApi(OpenApi.Version, info, paths, components = Components(schemas = registry.registeredSchemas))
  }
}
object RestMetadata extends RpcMetadataCompanion[RestMetadata] {
  private class Trie {
    val rpcChains: Map[HttpMethod, MBuffer[String]] =
      HttpMethod.values.mkMap(identity, _ => new MArrayBuffer[String])

    val byName: MMap[String, Trie] = new MHashMap
    var wildcard: Opt[Trie] = Opt.Empty

    def forPattern(pattern: List[PathPatternElement]): Trie = pattern match {
      case Nil => this
      case PathName(PathValue(pathName)) :: tail =>
        byName.getOrElseUpdate(pathName, new Trie).forPattern(tail)
      case PathParam(_) :: tail =>
        wildcard.getOrElse(new Trie().setup(t => wildcard = Opt(t))).forPattern(tail)
    }

    def fillWith(metadata: RestMetadata[_], prefixStack: List[(String, PrefixMetadata[_])] = Nil): Unit = {
      def prefixChain: String =
        prefixStack.reverseIterator.map({ case (k, _) => k }).mkStringOrEmpty("", "->", "->")

      metadata.prefixMethods.foreach { case entry@(rpcName, pm) =>
        if (prefixStack.contains(entry)) {
          throw new InvalidRestApiException(
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

sealed trait PathPatternElement
case class PathName(value: PathValue) extends PathPatternElement
case class PathParam(parameter: PathParamMetadata[_]) extends PathPatternElement

sealed abstract class RestMethodMetadata[T] extends TypedMetadata[T] {
  def methodPath: List[PathValue]
  def parametersMetadata: RestParametersMetadata

  val pathPattern: List[PathPatternElement] =
    methodPath.map(PathName) ++ parametersMetadata.path.flatMap(pp => PathParam(pp) :: pp.pathSuffix.map(PathName))

  def openapiPath: String =
    pathPattern.iterator.map {
      case PathName(PathValue(v)) => v
      case PathParam(param) => s"{${param.rpcName}}"
    }.mkString("/", "/", "")

  def applyPathParams(params: List[PathValue]): List[PathValue] = {
    def loop(params: List[PathValue], pattern: List[PathPatternElement]): List[PathValue] =
      (params, pattern) match {
        case (Nil, Nil) => Nil
        case (_, PathName(patternHead) :: patternTail) => patternHead :: loop(params, patternTail)
        case (param :: paramsTail, PathParam(_) :: patternTail) => param :: loop(paramsTail, patternTail)
        case _ => throw new IllegalArgumentException(
          s"got ${params.size} path params, expected ${parametersMetadata.path.size}")
      }
    loop(params, pathPattern)
  }

  def extractPathParams(path: List[PathValue]): Opt[(List[PathValue], List[PathValue])] = {
    def loop(path: List[PathValue], pattern: List[PathPatternElement]): Opt[(List[PathValue], List[PathValue])] =
      (path, pattern) match {
        case (pathTail, Nil) => Opt((Nil, pathTail))
        case (param :: pathTail, PathParam(_) :: patternTail) =>
          loop(pathTail, patternTail).map { case (params, tail) => (param :: params, tail) }
        case (pathHead :: pathTail, PathName(patternHead) :: patternTail) if pathHead == patternHead =>
          loop(pathTail, patternTail)
        case _ => Opt.Empty
      }
    loop(path, pathPattern)
  }
}

case class PrefixMetadata[T](
  @reifyAnnot methodTag: Prefix,
  @composite parametersMetadata: RestParametersMetadata,
  @checked @infer result: RestMetadata.Lazy[T]
) extends RestMethodMetadata[T] {
  def methodPath: List[PathValue] = PathValue.split(methodTag.path)

  def openapiOperations(resolver: SchemaResolver): Iterator[(String, HttpMethod, Operation)] = {
    val pathPrefix = openapiPath
    val prefixParams = parametersMetadata.openapiParameters(resolver)
    result.value.openapiOperations(resolver).map { case (path, httpMethod, operation) =>
      (pathPrefix + path, httpMethod, operation.copy(parameters = prefixParams ++ operation.parameters))
    }
  }
}

case class HttpMethodMetadata[T](
  @reifyAnnot methodTag: HttpMethodTag,
  @composite parametersMetadata: RestParametersMetadata,
  @multi @tagged[JsonBodyParam] @rpcParamMetadata bodyParams: Mapping[CommonParamMetadata[_]],
  @optional @encoded @tagged[Body] @rpcParamMetadata singleBodyParam: Opt[BodyParamMetadata[_]],
  @infer responseType: HttpResponseType[T]
) extends RestMethodMetadata[T] {
  val method: HttpMethod = methodTag.method
  val singleBody: Boolean = singleBodyParam.isDefined
  def methodPath: List[PathValue] = PathValue.split(methodTag.path)

  def openapiOperation(resolver: SchemaResolver): Operation = {
    val requestBody =
      singleBodyParam.map(_.restRequestBody.requestBody(resolver)).getOrElse {
        if (bodyParams.isEmpty) RefOr(RequestBody(content = Map.empty))
        else {
          val schema = Schema(`type` = DataType.Object,
            properties = bodyParams.iterator.map {
              case (name, pm) => (name, resolver.resolve(pm.restSchema))
            }.toMap,
            required = bodyParams.iterator.collect {
              case (name, pm) if !pm.hasFallbackValue => name
            }.toList
          )
          RefOr(RestRequestBody.jsonRequestBody(RefOr(schema)))
        }
      }
    Operation(
      responseType.responses(resolver),
      parameters = parametersMetadata.openapiParameters(resolver),
      requestBody = requestBody
    )
  }
}

@implicitNotFound("HttpResponseType for ${T} not found. It may be provided by appropriate RestSchema or RestResponses " +
  "instance (e.g. RestSchema[T] implies RestResponses[T] which implies HttpResponseType[Future[T]])")
case class HttpResponseType[T](responses: SchemaResolver => Responses)
object HttpResponseType {
  implicit def forFuture[T: RestResponses]: HttpResponseType[Future[T]] =
    HttpResponseType[Future[T]](RestResponses[T].responses)
}

case class RestParametersMetadata(
  @multi @tagged[Path] @rpcParamMetadata path: List[PathParamMetadata[_]],
  @multi @tagged[Header] @rpcParamMetadata headers: Mapping[CommonParamMetadata[_]],
  @multi @tagged[Query] @rpcParamMetadata query: Mapping[CommonParamMetadata[_]]
) {
  def openapiParameters(resolver: SchemaResolver): List[RefOr[Parameter]] = {
    val it = path.iterator.map(ppm => ppm.common.openapiParameter(resolver, ppm.rpcName, Location.Path)) ++
      headers.iterator.map({ case (name, pm) => pm.openapiParameter(resolver, name, Location.Header) }) ++
      query.iterator.map({ case (name, pm) => pm.openapiParameter(resolver, name, Location.Query) })
    it.map(RefOr(_)).toList
  }
}

case class CommonParamMetadata[T](
  @optional @reifyAnnot whenAbsent: Opt[whenAbsent[T]],
  @isAnnotated[transientDefault] transientDefault: Boolean,
  @reifyFlags flags: ParamFlags,
  @infer restSchema: RestSchema[T]
) extends TypedMetadata[T] {
  val hasFallbackValue: Boolean =
    whenAbsent.fold(flags.hasDefaultValue)(wa => Try(wa.value).isSuccess)

  def openapiParameter(resolver: SchemaResolver, name: String, in: Location): Parameter =
    Parameter(name, in, required = !hasFallbackValue, schema = resolver.resolve(restSchema))
}

case class PathParamMetadata[T](
  @composite common: CommonParamMetadata[T],
  @reifyName(useRawName = true) rpcName: String,
  @reifyAnnot pathAnnot: Path
) extends TypedMetadata[T] {
  val pathSuffix: List[PathValue] = PathValue.split(pathAnnot.pathSuffix)
}

case class BodyParamMetadata[T](
  @infer restRequestBody: RestRequestBody[T]
) extends TypedMetadata[T]

class InvalidRestApiException(msg: String) extends RestException(msg)
