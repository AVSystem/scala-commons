package com.avsystem.commons
package rest

import com.avsystem.commons.annotation.{AnnotationAggregate, defaultsToName}
import com.avsystem.commons.rpc._

import scala.annotation.StaticAnnotation

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
    * anything to URL path. Any special characters will be URL-encoded when creating HTTP request.
    * If path is not specified explicitly, method name is used (the actual method name, not `rpcName`).
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
class GET(val path: String = null) extends HttpMethodTag(HttpMethod.GET) {
  @rpcNamePrefix("get_", overloadedOnly = true) type Implied
}

/** See [[BodyMethodTag]] */
class POST(val path: String = null) extends BodyMethodTag(HttpMethod.POST) {
  @rpcNamePrefix("post_", overloadedOnly = true) type Implied
}
/** See [[BodyMethodTag]] */
class PATCH(val path: String = null) extends BodyMethodTag(HttpMethod.PATCH) {
  @rpcNamePrefix("patch_", overloadedOnly = true) type Implied
}
/** See [[BodyMethodTag]] */
class PUT(val path: String = null) extends BodyMethodTag(HttpMethod.PUT) {
  @rpcNamePrefix("put_", overloadedOnly = true) type Implied
}
/** See [[BodyMethodTag]] */
class DELETE(val path: String = null) extends BodyMethodTag(HttpMethod.DELETE) {
  @rpcNamePrefix("delete_", overloadedOnly = true) type Implied
}

/**
  * Base trait for tag annotations which specify how a HTTP body is built for invocation of particular
  * method.
  */
sealed trait BodyTypeTag extends RpcTag

/**
  * Indicates that a HTTP REST method takes no body. This annotation is assumed by default
  * for [[GET]] and [[Prefix]] methods. There should be no reason to use it explicitly.
  */
class NoBody extends BodyTypeTag

sealed trait SomeBodyTag extends BodyTypeTag

/**
  * Causes the [[Body]] parameters of a HTTP REST method to be encoded as `application/json`.
  * Each parameter value itself will be first serialized to [[JsonValue]].
  * This annotation only applies to methods which may include HTTP body (i.e. not [[GET]])
  * and is assumed by default, so there should be no reason to apply it explicitly.
  */
class JsonBody extends SomeBodyTag

/**
  * Causes the [[Body]] parameters of a HTTP REST method to be encoded as `application/x-www-form-urlencoded`.
  * Each parameter value itself will be first serialized to [[QueryValue]].
  * This annotation only applies to methods which may include HTTP body (i.e. not [[GET]]).
  */
class FormBody extends SomeBodyTag

/**
  * Requires that a method takes exactly one [[Body]] parameter which serializes directly into [[HttpBody]].
  * Serialization may then use arbitrary body format. This annotation only applies to methods which may
  * include HTTP body (i.e. not [[GET]]).
  */
class CustomBody extends SomeBodyTag

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
class Prefix(val path: String = null) extends RestMethodTag

sealed trait RestParamTag extends RpcTag
sealed trait NonBodyTag extends RestParamTag {
  def isPath: Boolean = this match {
    case _: Path => true
    case _ => false
  }
  def isHeader: Boolean = this match {
    case _: Header => true
    case _ => false
  }
  def isQuery: Boolean = this match {
    case _: Query => true
    case _ => false
  }
}

/**
  * REST method parameters annotated as [[Path]] will be encoded as [[PathValue]] and appended to URL path, in the
  * declaration order. Parameters of [[Prefix]] REST methods are interpreted as [[Path]] parameters by default.
  */
class Path(val pathSuffix: String = "") extends NonBodyTag

/**
  * REST method parameters annotated as [[Header]] will be encoded as [[HeaderValue]] and added to HTTP headers.
  * Header name must be explicitly given as argument of this annotation.
  */
class Header(override val name: String)
  extends rpcName(name) with NonBodyTag

/**
  * REST method parameters annotated as [[Query]] will be encoded as [[QueryValue]] and added to URL query
  * parameters. Parameters of [[GET]] REST methods are interpreted as [[Query]] parameters by default.
  */
class Query(@defaultsToName override val name: String = null)
  extends rpcName(name) with NonBodyTag

/**
  * REST method parameters annotated as [[Body]] will be used to build HTTP request body.
  * How exactly that happens depends on [[BodyTypeTag]] applied on a method. By default, [[JsonBody]] is assumed
  * which means that body parameters will be combined into a single JSON object sent as body.
  * Body parameters are allowed only in REST methods annotated as [[POST]], [[PATCH]], [[PUT]] or [[DELETE]].
  *
  * [[Body]] annotation is assumed by default for parameters of all methods which may include HTTP body.
  * This means that there's usually no reason to apply this annotation explicitly. It may only be useful when
  * wanting to customize JSON/form field name.
  */
class Body(@defaultsToName override val name: String = null)
  extends rpcName(name) with RestParamTag
