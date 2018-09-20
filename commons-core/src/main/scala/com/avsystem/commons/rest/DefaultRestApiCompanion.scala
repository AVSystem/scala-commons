package com.avsystem.commons
package rest

import com.avsystem.commons.misc.{ImplicitNotFound, MacroGenerated}
import com.avsystem.commons.rest.RawRest.{AsRawRealRpc, AsRawRpc, AsRealRpc}
import com.avsystem.commons.rest.openapi.{OpenApiMetadata, RestSchema, RestStructure}
import com.avsystem.commons.rpc.{AsRaw, AsRawReal, AsReal, Fallback, RpcMacroInstances}
import com.avsystem.commons.serialization.json.{JsonStringInput, JsonStringOutput}
import com.avsystem.commons.serialization.{GenCodec, GenKeyCodec, HasGenCodec}

import scala.annotation.implicitNotFound

/**
  * Base class for companion objects of ADTs (case classes, objects, sealed hierarchies) which are used as
  * parameter or result types in REST API traits. Automatically provides instances of
  * [[com.avsystem.commons.serialization.GenCodec GenCodec]] and
  * [[com.avsystem.commons.rest.openapi.RestSchema RestSchema]].
  *
  * @example
  * {{{
  *   case class User(id: String, name: String, birthYear: Int)
  *   object User extends RestDataCompanion[User]
  * }}}
  */
abstract class RestDataCompanion[T](implicit
  macroRestStructure: MacroGenerated[DefaultRestImplicits, RestStructure[T]],
  macroCodec: MacroGenerated[Any, GenCodec[T]]
) extends HasGenCodec[T] with DefaultRestImplicits {
  implicit lazy val restStructure: RestStructure[T] = macroRestStructure.forCompanion(this)
  implicit lazy val restSchema: RestSchema[T] = restStructure.standaloneSchema // lazy on restStructure
}

trait ClientInstances[Real] {
  def metadata: RestMetadata[Real]
  def asReal: AsRealRpc[Real]
}
trait ServerInstances[Real] {
  def metadata: RestMetadata[Real]
  def asRaw: AsRawRpc[Real]
}
trait OpenApiServerInstances[Real] extends ServerInstances[Real] {
  def openapiMetadata: OpenApiMetadata[Real]
}
trait FullInstances[Real] {
  def metadata: RestMetadata[Real]
  def asRawReal: AsRawRealRpc[Real]
}
trait OpenApiFullInstances[Real] extends FullInstances[Real] {
  def openapiMetadata: OpenApiMetadata[Real]
}

/** @see [[RestApiCompanion]]*/
abstract class RestClientApiCompanion[Implicits, Real](protected val implicits: Implicits)(
  implicit inst: RpcMacroInstances[Implicits, ClientInstances, Real]
) {
  implicit final lazy val restMetadata: RestMetadata[Real] = inst(implicits, this).metadata
  implicit final lazy val restAsReal: AsRealRpc[Real] = inst(implicits, this).asReal

  final def fromHandleRequest(handleRequest: RawRest.HandleRequest): Real =
    RawRest.fromHandleRequest(handleRequest)
}

/** @see [[RestApiCompanion]]*/
abstract class RestServerApiCompanion[Implicits, Real](protected val implicits: Implicits)(
  implicit inst: RpcMacroInstances[Implicits, ServerInstances, Real]
) {
  implicit final lazy val restMetadata: RestMetadata[Real] = inst(implicits, this).metadata
  implicit final lazy val restAsRaw: AsRawRpc[Real] = inst(implicits, this).asRaw

  final def asHandleRequest(real: Real): RawRest.HandleRequest =
    RawRest.asHandleRequest(real)
}

/** @see [[RestApiCompanion]]*/
abstract class RestServerOpenApiCompanion[Implicits, Real](protected val implicits: Implicits)(
  implicit inst: RpcMacroInstances[Implicits, OpenApiServerInstances, Real]
) {
  implicit final lazy val restMetadata: RestMetadata[Real] = inst(implicits, this).metadata
  implicit final lazy val restAsRaw: AsRawRpc[Real] = inst(implicits, this).asRaw
  implicit final lazy val openapiMetadata: OpenApiMetadata[Real] = inst(implicits, this).openapiMetadata

  final def asHandleRequest(real: Real): RawRest.HandleRequest =
    RawRest.asHandleRequest(real)
}

/**
  * Base class for REST trait companions. Reduces boilerplate needed in order to define appropriate instances
  * of `AsRawReal` and `RestMetadata` for given trait. The `Implicits` type parameter lets you inject additional implicits
  * into macro materialization of these instances, e.g. [[DefaultRestImplicits]].
  * Usually, for even less boilerplate, this base class is extended by yet another abstract class which fixes
  * the `Implicits` type, e.g. [[DefaultRestApiCompanion]].
  */
abstract class RestApiCompanion[Implicits, Real](protected val implicits: Implicits)(
  implicit inst: RpcMacroInstances[Implicits, FullInstances, Real]
) {
  implicit final lazy val restMetadata: RestMetadata[Real] = inst(implicits, this).metadata
  implicit final lazy val restAsRawReal: AsRawRealRpc[Real] = inst(implicits, this).asRawReal

  final def fromHandleRequest(handleRequest: RawRest.HandleRequest): Real =
    RawRest.fromHandleRequest(handleRequest)
  final def asHandleRequest(real: Real): RawRest.HandleRequest =
    RawRest.asHandleRequest(real)
}

/** @see [[RestApiCompanion]]*/
abstract class RestOpenApiCompanion[Implicits, Real](protected val implicits: Implicits)(
  implicit inst: RpcMacroInstances[Implicits, OpenApiFullInstances, Real]
) {
  implicit final lazy val restMetadata: RestMetadata[Real] = inst(implicits, this).metadata
  implicit final lazy val restAsRawReal: AsRawRealRpc[Real] = inst(implicits, this).asRawReal
  implicit final lazy val openapiMetadata: OpenApiMetadata[Real] = inst(implicits, this).openapiMetadata

  final def fromHandleRequest(handleRequest: RawRest.HandleRequest): Real =
    RawRest.fromHandleRequest(handleRequest)
  final def asHandleRequest(real: Real): RawRest.HandleRequest =
    RawRest.asHandleRequest(real)
}

/**
  * Defines [[com.avsystem.commons.serialization.GenCodec GenCodec]] and
  * [[com.avsystem.commons.serialization.GenKeyCodec GenKeyCodec]] based serialization for REST API traits.
  */
trait DefaultRestImplicits extends FloatingPointRestImplicits {
  // Implicits wrapped into `Fallback` so that they don't get higher priority just because they're imported
  // This way concrete classes may override these implicits with implicits in their companion objects
  implicit def pathValueFallbackAsRealRaw[T: GenKeyCodec]: Fallback[AsRawReal[PathValue, T]] =
    Fallback(AsRawReal.create(v => PathValue(GenKeyCodec.write[T](v)), v => GenKeyCodec.read[T](v.value)))
  implicit def headerValueDefaultAsRealRaw[T: GenKeyCodec]: Fallback[AsRawReal[HeaderValue, T]] =
    Fallback(AsRawReal.create(v => HeaderValue(GenKeyCodec.write[T](v)), v => GenKeyCodec.read[T](v.value)))
  implicit def queryValueDefaultAsRealRaw[T: GenKeyCodec]: Fallback[AsRawReal[QueryValue, T]] =
    Fallback(AsRawReal.create(v => QueryValue(GenKeyCodec.write[T](v)), v => GenKeyCodec.read[T](v.value)))
  implicit def jsonValueDefaultAsRealRaw[T: GenCodec]: Fallback[AsRawReal[JsonValue, T]] =
    Fallback(AsRawReal.create(v => JsonValue(JsonStringOutput.write[T](v)), v => JsonStringInput.read[T](v.value)))

  @implicitNotFound("Cannot serialize ${T} into HttpBody. This may be caused by lack of GenCodec instance for ${T}")
  implicit def asRawNotFound[T]: ImplicitNotFound[AsRaw[HttpBody, T]] = ImplicitNotFound()

  @implicitNotFound("Cannot deserialize ${T} from HttpBody. This may be caused by lack of GenCodec instance for ${T}")
  implicit def asRealNotFound[T]: ImplicitNotFound[AsReal[HttpBody, T]] = ImplicitNotFound()

  @implicitNotFound("Cannot serialize ${T} into RestResponse. This may be caused by lack of GenCodec instance for ${T}")
  implicit def futureAsRawNotFound[T]: ImplicitNotFound[AsRaw[RawRest.Async[RestResponse], Try[Future[T]]]] =
    ImplicitNotFound()

  @implicitNotFound("Cannot deserialize ${T} from RestResponse. This may be caused by lack of GenCodec instance for ${T}")
  implicit def futureAsRealNotFound[T]: ImplicitNotFound[AsReal[RawRest.Async[RestResponse], Try[Future[T]]]] =
    ImplicitNotFound()
}
object DefaultRestImplicits extends DefaultRestImplicits

/**
  * Base class for companions of REST API traits used only for REST clients to external services.
  * Injects `GenCodec` and `GenKeyCodec` based serialization.
  */
abstract class DefaultRestClientApiCompanion[Real](implicit
  inst: RpcMacroInstances[DefaultRestImplicits, ClientInstances, Real]
) extends RestClientApiCompanion[DefaultRestImplicits, Real](DefaultRestImplicits)

/**
  * Base class for companions of REST API traits used only for REST servers exposed to external world.
  * Injects `GenCodec` and `GenKeyCodec` based serialization and forces derivation of
  * [[com.avsystem.commons.rest.openapi.OpenApiMetadata OpenApiMetadata]].
  */
abstract class DefaultRestServerApiCompanion[Real](implicit
  inst: RpcMacroInstances[DefaultRestImplicits, OpenApiServerInstances, Real]
) extends RestServerOpenApiCompanion[DefaultRestImplicits, Real](DefaultRestImplicits)

/**
  * Base class for companions of REST API traits used for both REST clients and servers.
  * Injects `GenCodec` and `GenKeyCodec` based serialization and forces derivation of
  * [[com.avsystem.commons.rest.openapi.OpenApiMetadata OpenApiMetadata]].
  */
abstract class DefaultRestApiCompanion[Real](implicit
  inst: RpcMacroInstances[DefaultRestImplicits, OpenApiFullInstances, Real]
) extends RestOpenApiCompanion[DefaultRestImplicits, Real](DefaultRestImplicits)
