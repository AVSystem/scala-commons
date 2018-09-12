package com.avsystem.commons
package rest.openapi

import com.avsystem.commons.meta.{multi, reifyAnnot, _}
import com.avsystem.commons.rest.{Header => HeaderAnnot, _}
import com.avsystem.commons.rpc._
import com.avsystem.commons.serialization.{transientDefault, whenAbsent}

@methodTag[RestMethodTag]
case class OpenApiMetadata[T](
  @multi @tagged[Prefix](whenUntagged = new Prefix)
  @paramTag[RestParamTag](defaultTag = new Path)
  @rpcMethodMetadata
  prefixes: List[OpenApiPrefix[_]],

  @multi @tagged[GET]
  @paramTag[RestParamTag](defaultTag = new Query)
  @rpcMethodMetadata
  gets: List[OpenApiGetOperation[_]],

  @multi @tagged[BodyMethodTag](whenUntagged = new POST)
  @paramTag[RestParamTag](defaultTag = new JsonBodyParam)
  @rpcMethodMetadata
  bodyMethods: List[OpenApiBodyOperation[_]]
) {
  val httpMethods: List[OpenApiOperation[_]] = (gets: List[OpenApiOperation[_]]) ++ bodyMethods

  def operations(resolver: SchemaResolver): Iterator[(String, HttpMethod, Operation)] =
    prefixes.iterator.flatMap(_.operations(resolver)) ++
      httpMethods.iterator.map(m => (m.pathPattern, m.methodTag.method, m.operation(resolver)))

  def paths(resolver: SchemaResolver): Paths = {
    val pathsMap = new MLinkedHashMap[String, MLinkedHashMap[HttpMethod, Operation]]
    operations(resolver).foreach {
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
        delete = ops.getOpt(HttpMethod.DELETE).toOptArg
      )
      (path, RefOr(pathItem))
    }.toMap)
  }

  def openapi(
    info: Info,
    servers: List[Server] = Nil,
    security: List[SecurityRequirement] = Nil,
    tags: List[Tag] = Nil,
    externalDocs: OptArg[ExternalDocumentation] = OptArg.Empty
  ): OpenApi = {
    val registry = new SchemaRegistry(n => s"#/components/schemas/$n")
    OpenApi(OpenApi.Version,
      info,
      paths(registry),
      components = Components(schemas = registry.registeredSchemas),
      servers = servers,
      security = security,
      tags = tags,
      externalDocs = externalDocs
    )
  }
}
object OpenApiMetadata extends RpcMetadataCompanion[OpenApiMetadata]

sealed trait OpenApiMethod[T] extends TypedMetadata[T] {
  @reifyName(useRawName = true) def name: String
  @reifyAnnot def methodTag: RestMethodTag
  @multi
  @rpcParamMetadata
  @tagged[NonBodyTag] def parameters: List[OpenApiParameter[_]]

  val pathPattern: String = {
    val pathParts = methodTag.path :: parameters.flatMap {
      case OpenApiParameter(path: Path, info, _) =>
        s"{${info.name}}" :: path.pathSuffix :: Nil
      case _ => Nil
    }
    pathParts.iterator.map(_.stripPrefix("/").stripSuffix("/")).filter(_.nonEmpty).mkString("/", "/", "")
  }
}

case class OpenApiPrefix[T](
  name: String,
  methodTag: Prefix,
  parameters: List[OpenApiParameter[_]],
  @infer @checked result: OpenApiMetadata.Lazy[T]
) extends OpenApiMethod[T] {

  def operations(resolver: SchemaResolver): Iterator[(String, HttpMethod, Operation)] = {
    val prefixParams = parameters.map(_.parameter(resolver))
    result.value.operations(resolver).map { case (path, httpMethod, operation) =>
      (pathPattern + path, httpMethod, operation.copy(
        operationId = operation.operationId.toOpt.map(oid => s"${name}_$oid").toOptArg,
        parameters = prefixParams ++ operation.parameters
      ))
    }
  }
}

sealed trait OpenApiOperation[T] extends OpenApiMethod[T] {
  @infer
  @checked def responseType: HttpResponseType[T]
  @multi
  @reifyAnnot def adjusters: List[OperationAdjuster]
  def methodTag: HttpMethodTag
  def requestBody(resolver: SchemaResolver): Opt[RefOr[RequestBody]]

  def operation(resolver: SchemaResolver): Operation = {
    val op = Operation(
      responseType.responses(resolver),
      operationId = name,
      parameters = parameters.map(_.parameter(resolver)),
      requestBody = requestBody(resolver).toOptArg
    )
    adjusters.foldRight(op)(_ adjustOperation _)
  }
}

case class OpenApiGetOperation[T](
  name: String,
  methodTag: HttpMethodTag,
  parameters: List[OpenApiParameter[_]],
  responseType: HttpResponseType[T],
  adjusters: List[OperationAdjuster]
) extends OpenApiOperation[T] {
  def requestBody(resolver: SchemaResolver): Opt[RefOr[RequestBody]] = Opt.Empty
}

case class OpenApiBodyOperation[T](
  name: String,
  methodTag: HttpMethodTag,
  parameters: List[OpenApiParameter[_]],
  @multi @rpcParamMetadata @tagged[JsonBodyParam] bodyParams: List[OpenApiParamInfo[_]],
  @optional @encoded @rpcParamMetadata @tagged[Body] singleBody: Opt[OpenApiBody[_]],
  responseType: HttpResponseType[T],
  adjusters: List[OperationAdjuster]
) extends OpenApiOperation[T] {

  def requestBody(resolver: SchemaResolver): Opt[RefOr[RequestBody]] =
    singleBody.map(_.requestBody.requestBody(resolver).opt).getOrElse {
      if (bodyParams.isEmpty) Opt.Empty else Opt {
        val schema = Schema(`type` = DataType.Object,
          properties = bodyParams.iterator.map(p => (p.name, resolver.resolve(p.restSchema))).toMap,
          required = bodyParams.collect { case p if !p.hasFallbackValue => p.name }
        )
        RefOr(RestRequestBody.jsonRequestBody(RefOr(schema)))
      }
    }
}

case class OpenApiParamInfo[T](
  @reifyName(useRawName = true) name: String,
  @optional @reifyAnnot whenAbsent: Opt[whenAbsent[T]],
  @isAnnotated[transientDefault] transientDefault: Boolean,
  @reifyFlags flags: ParamFlags,
  @infer restSchema: RestSchema[T]
) extends TypedMetadata[T] {
  val hasFallbackValue: Boolean =
    whenAbsent.fold(flags.hasDefaultValue)(wa => Try(wa.value).isSuccess)
}

case class OpenApiParameter[T](
  @reifyAnnot paramTag: NonBodyTag,
  @composite info: OpenApiParamInfo[T],
  @multi @reifyAnnot adjusters: List[ParameterAdjuster]
) extends TypedMetadata[T] {

  def parameter(resolver: SchemaResolver): RefOr[Parameter] = {
    val in = paramTag match {
      case _: Path => Location.Path
      case _: HeaderAnnot => Location.Header
      case _: Query => Location.Query
    }
    val param = Parameter(info.name, in, required = !info.hasFallbackValue, schema = resolver.resolve(info.restSchema))
    RefOr(adjusters.foldRight(param)(_ adjustParameter _))
  }
}

case class OpenApiBody[T](@infer requestBody: RestRequestBody[T]) extends TypedMetadata[T]

