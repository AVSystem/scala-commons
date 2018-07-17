package com.avsystem.commons
package rest

import com.avsystem.commons.rest.RawRest.{AsRawRealRpc, AsRawRpc, AsRealRpc}
import com.avsystem.commons.rpc.{AsRawReal, RpcMacroInstances}
import com.avsystem.commons.serialization.json.{JsonStringInput, JsonStringOutput}
import com.avsystem.commons.serialization.{GenCodec, GenKeyCodec}

trait ClientInstances[Real, I] extends RpcMacroInstances[Real] {
  type Implicits = I
  def metadata(implicits: I): RestMetadata[Real]
  def asReal(implicits: I): AsRealRpc[Real]
}
object ClientInstances {
  implicit def materialize[Real, I]: ClientInstances[Real, I] =
  macro macros.rpc.RpcMacros.macroInstances[ClientInstances[Real, I], Real]
}
trait ServerInstances[Real, I] extends RpcMacroInstances[Real] {
  type Implicits = I
  def metadata(implicits: I): RestMetadata[Real]
  def asRaw(implicits: I): AsRawRpc[Real]
}
object ServerInstances {
  implicit def materialize[Real, I]: ServerInstances[Real, I] =
  macro macros.rpc.RpcMacros.macroInstances[ServerInstances[Real, I], Real]
}
trait FullInstances[Real, I] extends RpcMacroInstances[Real] {
  type Implicits = I
  def metadata(implicits: I): RestMetadata[Real]
  def asRawReal(implicits: I): AsRawRealRpc[Real]
}
object FullInstances {
  implicit def clientServerInstances[Real, I]: FullInstances[Real, I] =
  macro macros.rpc.RpcMacros.macroInstances[FullInstances[Real, I], Real]
}

/** @see [[FullApiCompanion]] */
abstract class ClientApiCompanion[Real, I](implicit instances: ClientInstances[Real, I]) { this: I =>
  implicit final lazy val restMetadata: RestMetadata[Real] = instances.metadata(this)
  implicit final lazy val restAsReal: AsRealRpc[Real] = instances.asReal(this)
}
/** @see [[FullApiCompanion]] */
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

/**
  * Defines [[GenCodec]] and [[GenKeyCodec]] based serialization for REST API traits.
  */
trait DefaultRestImplicits {
  implicit def pathValueDefaultAsRealRaw[T: GenKeyCodec]: AsRawReal[PathValue, T] =
    AsRawReal.create(v => PathValue(GenKeyCodec.write[T](v)), v => GenKeyCodec.read[T](v.value))
  implicit def headerValueDefaultAsRealRaw[T: GenKeyCodec]: AsRawReal[HeaderValue, T] =
    AsRawReal.create(v => HeaderValue(GenKeyCodec.write[T](v)), v => GenKeyCodec.read[T](v.value))
  implicit def queryValueDefaultAsRealRaw[T: GenKeyCodec]: AsRawReal[QueryValue, T] =
    AsRawReal.create(v => QueryValue(GenKeyCodec.write[T](v)), v => GenKeyCodec.read[T](v.value))
  implicit def jsonValueDefaultAsRealRaw[T: GenCodec]: AsRawReal[JsonValue, T] =
    AsRawReal.create(v => JsonValue(JsonStringOutput.write[T](v)), v => JsonStringInput.read[T](v.value))
}
object DefaultRestImplicits extends DefaultRestImplicits

/**
  * Base class for companions of REST API traits used only for REST clients to external services.
  * Injects [[GenCodec]] and [[GenKeyCodec]] based serialization.
  */
abstract class RestClientApiCompanion[Real](
  implicit instances: ClientInstances[Real, DefaultRestImplicits]
) extends ClientApiCompanion[Real, DefaultRestImplicits] with DefaultRestImplicits

/**
  * Base class for companions of REST API traits used only for REST servers exposed to external world.
  * Injects [[GenCodec]] and [[GenKeyCodec]] based serialization.
  */
abstract class RestServerApiCompanion[Real](
  implicit instances: ServerInstances[Real, DefaultRestImplicits]
) extends ServerApiCompanion[Real, DefaultRestImplicits] with DefaultRestImplicits

/**
  * Base class for companions of REST API traits used for both REST clients and servers.
  * Injects [[GenCodec]] and [[GenKeyCodec]] based serialization.
  */
abstract class RestApiCompanion[Real](
  implicit instances: FullInstances[Real, DefaultRestImplicits]
) extends FullApiCompanion[Real, DefaultRestImplicits] with DefaultRestImplicits
