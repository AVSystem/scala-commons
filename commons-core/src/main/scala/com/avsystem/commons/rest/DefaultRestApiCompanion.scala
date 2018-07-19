package com.avsystem.commons
package rest

import com.avsystem.commons.rest.RawRest.{AsRawRealRpc, AsRawRpc, AsRealRpc}
import com.avsystem.commons.rpc.{AsRawReal, Fallback, RpcMacroInstances}
import com.avsystem.commons.serialization.json.{JsonStringInput, JsonStringOutput}
import com.avsystem.commons.serialization.{GenCodec, GenKeyCodec}

trait ClientInstances[Real] {
  def metadata: RestMetadata[Real]
  def asReal: AsRealRpc[Real]
}
trait ServerInstances[Real] {
  def metadata: RestMetadata[Real]
  def asRaw: AsRawRpc[Real]
}
trait FullInstances[Real] {
  def metadata: RestMetadata[Real]
  def asRawReal: AsRawRealRpc[Real]
}

/** @see [[RestApiCompanion]]*/
abstract class RestClientApiCompanion[Implicits, Real](implicits: Implicits)(
  implicit inst: RpcMacroInstances[Implicits, ClientInstances, Real]
) {
  implicit final lazy val restMetadata: RestMetadata[Real] = inst(implicits).metadata
  implicit final lazy val restAsReal: AsRealRpc[Real] = inst(implicits).asReal
}

/** @see [[RestApiCompanion]]*/
abstract class RestServerApiCompanion[Implicits, Real](implicits: Implicits)(
  implicit inst: RpcMacroInstances[Implicits, ServerInstances, Real]
) {
  implicit final lazy val restMetadata: RestMetadata[Real] = inst(implicits).metadata
  implicit final lazy val restAsRaw: AsRawRpc[Real] = inst(implicits).asRaw
}

/**
  * Base class for REST trait companions. Reduces boilerplate needed in order to define appropriate instances
  * of `AsRawReal` and `RestMetadata` for given trait. The `Implicits` type parameter lets you inject additional implicits
  * into macro materialization of these instances, e.g. [[DefaultRestImplicits]].
  * Usually, for even less boilerplate, this base class is extended by yet another abstract class which fixes
  * the `Implicits` type, e.g. [[DefaultRestApiCompanion]].
  */
abstract class RestApiCompanion[Implicits, Real](implicits: Implicits)(
  implicit inst: RpcMacroInstances[Implicits, FullInstances, Real]
) {
  implicit final lazy val restMetadata: RestMetadata[Real] = inst(implicits).metadata
  implicit final lazy val restAsRawReal: AsRawRealRpc[Real] = inst(implicits).asRawReal
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
}
object DefaultRestImplicits extends DefaultRestImplicits

/**
  * Base class for companions of REST API traits used only for REST clients to external services.
  * Injects `GenCodec` and `GenKeyCodec` based serialization.
  */
abstract class DefaultRestClientApiCompanion[Real](implicit inst: RpcMacroInstances[DefaultRestImplicits, ClientInstances, Real])
  extends RestClientApiCompanion[DefaultRestImplicits, Real](DefaultRestImplicits)

/**
  * Base class for companions of REST API traits used only for REST servers exposed to external world.
  * Injects `GenCodec` and `GenKeyCodec` based serialization.
  */
abstract class DefaultRestServerApiCompanion[Real](implicit inst: RpcMacroInstances[DefaultRestImplicits, ServerInstances, Real])
  extends RestServerApiCompanion[DefaultRestImplicits, Real](DefaultRestImplicits)

/**
  * Base class for companions of REST API traits used for both REST clients and servers.
  * Injects `GenCodec` and `GenKeyCodec` based serialization.
  */
abstract class DefaultRestApiCompanion[Real](implicit inst: RpcMacroInstances[DefaultRestImplicits, FullInstances, Real])
  extends RestApiCompanion[DefaultRestImplicits, Real](DefaultRestImplicits)
