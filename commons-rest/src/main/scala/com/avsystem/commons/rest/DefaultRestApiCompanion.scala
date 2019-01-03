package com.avsystem.commons
package rest

import com.avsystem.commons.concurrent.Async
import com.avsystem.commons.meta.{Fallback, MacroInstances}
import com.avsystem.commons.misc.ImplicitNotFound
import com.avsystem.commons.rest.RawRest.{AsRawRpc, AsRealRpc}
import com.avsystem.commons.rest.openapi.{OpenApiMetadata, RestResponses, RestResultType, RestSchema, RestStructure}
import com.avsystem.commons.rpc.{AsRaw, AsRawReal, AsReal, InvalidRpcCall}
import com.avsystem.commons.serialization.json.{JsonStringInput, JsonStringOutput}
import com.avsystem.commons.serialization.{GenCodec, GenKeyCodec}

import scala.annotation.implicitNotFound

trait CodecWithStructure[T] {
  def codec: GenCodec[T]
  def structure: RestStructure[T]
}

/**
  * Base class for companion objects of ADTs (case classes, objects, sealed hierarchies) which are used as
  * parameter or result types in REST API traits. Automatically provides instances of `GenCodec` and `RestSchema`.
  *
  * @example
  * {{{
  *   case class User(id: String, name: String, birthYear: Int)
  *   object User extends RestDataCompanion[User]
  * }}}
  */
abstract class RestDataCompanion[T](implicit
  instances: MacroInstances[DefaultRestImplicits, CodecWithStructure[T]]
) extends {
  implicit lazy val codec: GenCodec[T] = instances(DefaultRestImplicits, this).codec
  implicit lazy val restStructure: RestStructure[T] = instances(DefaultRestImplicits, this).structure
  implicit lazy val restSchema: RestSchema[T] = RestSchema.lazySchema(restStructure.standaloneSchema)
}

trait ClientInstances[Real] {
  def asReal: AsRealRpc[Real]
  def metadata: RestMetadata[Real]
}
trait ServerInstances[Real] {
  def asRaw: AsRawRpc[Real]
  def metadata: RestMetadata[Real]
}
trait FullInstances[Real] extends ServerInstances[Real] with ClientInstances[Real]

trait OpenApiInstances[Real] {
  def openapiMetadata: OpenApiMetadata[Real]
}
trait OpenApiServerInstances[Real] extends ServerInstances[Real] with OpenApiInstances[Real]
trait OpenApiFullInstances[Real] extends FullInstances[Real] with OpenApiInstances[Real]

/** @see [[RestApiCompanion]] */
abstract class RestClientApiCompanion[Implicits, Real](protected val implicits: Implicits)(
  implicit inst: MacroInstances[Implicits, ClientInstances[Real]]
) {
  implicit final lazy val restMetadata: RestMetadata[Real] = inst(implicits, this).metadata
  implicit final lazy val restAsReal: AsRealRpc[Real] = inst(implicits, this).asReal

  final def fromHandleRequest(handleRequest: RawRest.HandleRequest): Real =
    RawRest.fromHandleRequest(handleRequest)
}

/** @see [[RestApiCompanion]] */
abstract class RestServerApiCompanion[Implicits, Real](protected val implicits: Implicits)(
  implicit inst: MacroInstances[Implicits, ServerInstances[Real]]
) {
  implicit final lazy val restMetadata: RestMetadata[Real] = inst(implicits, this).metadata
  implicit final lazy val restAsRaw: AsRawRpc[Real] = inst(implicits, this).asRaw

  final def asHandleRequest(real: Real): RawRest.HandleRequest =
    RawRest.asHandleRequest(real)
}

/** @see [[RestApiCompanion]] */
abstract class RestServerOpenApiCompanion[Implicits, Real](protected val implicits: Implicits)(
  implicit inst: MacroInstances[Implicits, OpenApiServerInstances[Real]]
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
  implicit inst: MacroInstances[Implicits, FullInstances[Real]]
) {
  implicit final lazy val restMetadata: RestMetadata[Real] = inst(implicits, this).metadata
  implicit final lazy val restAsRaw: AsRawRpc[Real] = inst(implicits, this).asRaw
  implicit final lazy val restAsReal: AsRealRpc[Real] = inst(implicits, this).asReal

  final def fromHandleRequest(handleRequest: RawRest.HandleRequest): Real =
    RawRest.fromHandleRequest(handleRequest)
  final def asHandleRequest(real: Real): RawRest.HandleRequest =
    RawRest.asHandleRequest(real)
}

/** @see [[RestApiCompanion]] */
abstract class RestOpenApiCompanion[Implicits, Real](protected val implicits: Implicits)(
  implicit inst: MacroInstances[Implicits, OpenApiFullInstances[Real]]
) {
  implicit final lazy val restMetadata: RestMetadata[Real] = inst(implicits, this).metadata
  implicit final lazy val restAsRaw: AsRawRpc[Real] = inst(implicits, this).asRaw
  implicit final lazy val restAsReal: AsRealRpc[Real] = inst(implicits, this).asReal
  implicit final lazy val openapiMetadata: OpenApiMetadata[Real] = inst(implicits, this).openapiMetadata

  final def fromHandleRequest(handleRequest: RawRest.HandleRequest): Real =
    RawRest.fromHandleRequest(handleRequest)
  final def asHandleRequest(real: Real): RawRest.HandleRequest =
    RawRest.asHandleRequest(real)
}

trait FutureRestImplicits {
  implicit def futureToAsyncResp[T](
    implicit respAsRaw: AsRaw[RestResponse, T]
  ): AsRaw[Async[RestResponse], Try[Future[T]]] =
    AsRaw.create { triedFuture =>
      val future = triedFuture.fold(Future.failed, identity)
      callback => future.onCompleteNow(t => callback(t.map(respAsRaw.asRaw).recoverHttpError))
    }

  implicit def futureFromAsyncResp[T](
    implicit respAsReal: AsReal[RestResponse, T]
  ): AsReal[Async[RestResponse], Try[Future[T]]] =
    AsReal.create { async =>
      val promise = Promise[T]
      async(t => promise.complete(t.map(respAsReal.asReal)))
      Success(promise.future)
    }

  @implicitNotFound("#{forResponse}")
  implicit def futureAsRawNotFound[T](
    implicit forResponse: ImplicitNotFound[AsRaw[RestResponse, T]]
  ): ImplicitNotFound[AsRaw[Async[RestResponse], Try[Future[T]]]] = ImplicitNotFound()

  @implicitNotFound("#{forResponse}")
  implicit def futureAsRealNotFound[T](
    implicit forResponse: ImplicitNotFound[AsReal[RestResponse, T]]
  ): ImplicitNotFound[AsReal[Async[RestResponse], Try[Future[T]]]] = ImplicitNotFound()

  implicit def futureHttpResponseType[T]: HttpResponseType[Future[T]] =
    HttpResponseType[Future[T]]()

  @implicitNotFound("${T} is not a valid REST HTTP method result type - it must be wrapped into a Future")
  implicit def httpResponseTypeNotFound[T]: ImplicitNotFound[HttpResponseType[T]] =
    ImplicitNotFound()

  implicit def futureRestResultType[T: RestResponses]: RestResultType[Future[T]] =
    RestResultType[Future[T]](RestResponses[T].responses)

  @implicitNotFound("#{forRestResponses}")
  implicit def futureRestResultTypeNotFound[T](
    implicit forRestResponses: ImplicitNotFound[RestResponses[T]]
  ): ImplicitNotFound[RestResultType[Future[T]]] = ImplicitNotFound()
}
object FutureRestImplicits extends FutureRestImplicits

/**
  * Defines `GenCodec` and
  * `GenKeyCodec` based serialization for REST API traits.
  */
trait GenCodecRestImplicits extends FloatingPointRestImplicits {
  // read failure handling is now baked into macro-generated RPC `AsRaw` implementations but this
  // method is left for backwards compatibility - for instances materialized with previous version of macro
  protected final def handleReadFailure[T](expr: => T): T =
    try expr catch {
      case e: InvalidRpcCall => throw e
      case NonFatal(cause) => throw new InvalidRpcCall(cause.getMessage, cause)
    }

  // Implicits wrapped into `Fallback` so that they don't get higher priority just because they're imported
  // This way concrete classes may override these implicits with implicits in their companion objects
  implicit def pathValueFallbackAsRealRaw[T: GenKeyCodec]: Fallback[AsRawReal[PathValue, T]] =
    Fallback(AsRawReal.create(
      v => PathValue(GenKeyCodec.write[T](v)),
      v => handleReadFailure(GenKeyCodec.read[T](v.value))
    ))
  implicit def headerValueDefaultAsRealRaw[T: GenKeyCodec]: Fallback[AsRawReal[HeaderValue, T]] =
    Fallback(AsRawReal.create(
      v => HeaderValue(GenKeyCodec.write[T](v)),
      v => handleReadFailure(GenKeyCodec.read[T](v.value))
    ))
  implicit def queryValueDefaultAsRealRaw[T: GenKeyCodec]: Fallback[AsRawReal[QueryValue, T]] =
    Fallback(AsRawReal.create(
      v => QueryValue(GenKeyCodec.write[T](v)),
      v => handleReadFailure(GenKeyCodec.read[T](v.value))
    ))

  implicit def jsonValueDefaultAsRealRaw[T: GenCodec]: Fallback[AsRawReal[JsonValue, T]] =
    Fallback(AsRawReal.create(
      v => JsonValue(JsonStringOutput.write[T](v)),
      v => handleReadFailure(JsonStringInput.read[T](v.value))
    ))

  @implicitNotFound("Cannot serialize ${T} into JsonValue, probably because: #{forGenCodec}")
  implicit def asRawJsonNotFound[T](
    implicit forGenCodec: ImplicitNotFound[GenCodec[T]]
  ): ImplicitNotFound[AsRaw[JsonValue, T]] = ImplicitNotFound()

  @implicitNotFound("Cannot deserialize ${T} from JsonValue, probably because: #{forGenCodec}")
  implicit def asRealJsonNotFound[T](
    implicit forGenCodec: ImplicitNotFound[GenCodec[T]]
  ): ImplicitNotFound[AsReal[JsonValue, T]] = ImplicitNotFound()
}
object GenCodecRestImplicits extends GenCodecRestImplicits

trait DefaultRestImplicits extends FutureRestImplicits with GenCodecRestImplicits {
  @implicitNotFound("${T} is not a valid server REST API trait, does its companion extend " +
    "DefaultRestApiCompanion or DefaultRestServerApiCompanion?")
  implicit def rawRestAsRawNotFound[T]: ImplicitNotFound[AsRaw[RawRest, T]] = ImplicitNotFound()

  @implicitNotFound("${T} is not a valid client REST API trait, does its companion extend " +
    "DefaultRestApiCompanion or DefaultRestClientApiCompanion?")
  implicit def rawRestAsRealNotFound[T]: ImplicitNotFound[AsReal[RawRest, T]] = ImplicitNotFound()

  @implicitNotFound("RestSchema for ${T} not found. To provide it for case classes and sealed hierarchies " +
    "use RestDataCompanion (which also provides GenCodec)")
  implicit def schemaNotFound[T]: ImplicitNotFound[RestSchema[T]] = ImplicitNotFound()
}
object DefaultRestImplicits extends DefaultRestImplicits

/**
  * Base class for companions of REST API traits used only for REST clients to external services.
  * Injects `GenCodec` and `GenKeyCodec` based serialization.
  */
abstract class DefaultRestClientApiCompanion[Real](implicit
  inst: MacroInstances[DefaultRestImplicits, ClientInstances[Real]]
) extends RestClientApiCompanion[DefaultRestImplicits, Real](DefaultRestImplicits)

/**
  * Base class for companions of REST API traits used only for REST servers exposed to external world.
  * Injects `GenCodec` and `GenKeyCodec` based serialization and forces derivation of
  * `OpenApiMetadata`.
  */
abstract class DefaultRestServerApiCompanion[Real](implicit
  inst: MacroInstances[DefaultRestImplicits, OpenApiServerInstances[Real]]
) extends RestServerOpenApiCompanion[DefaultRestImplicits, Real](DefaultRestImplicits)

/**
  * Base class for companions of REST API traits used for both REST clients and servers.
  * Injects `GenCodec` and `GenKeyCodec` based serialization and forces derivation of
  * `OpenApiMetadata`.
  */
abstract class DefaultRestApiCompanion[Real](implicit
  inst: MacroInstances[DefaultRestImplicits, OpenApiFullInstances[Real]]
) extends RestOpenApiCompanion[DefaultRestImplicits, Real](DefaultRestImplicits)
