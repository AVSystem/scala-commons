package com.avsystem.commons
package rest

import com.avsystem.commons.rpc.AsRawReal
import com.avsystem.commons.serialization.json.{JsonStringInput, JsonStringOutput}
import com.avsystem.commons.serialization.{GenCodec, GenKeyCodec}

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
  implicit instances: RawRest.ClientInstances[Real, DefaultRestImplicits]
) extends RawRest.ClientApiCompanion[Real, DefaultRestImplicits] with DefaultRestImplicits

/**
  * Base class for companions of REST API traits used only for REST servers exposed to external world.
  * Injects [[GenCodec]] and [[GenKeyCodec]] based serialization.
  */
abstract class RestServerApiCompanion[Real](
  implicit instances: RawRest.ServerInstances[Real, DefaultRestImplicits]
) extends RawRest.ServerApiCompanion[Real, DefaultRestImplicits] with DefaultRestImplicits

/**
  * Base class for companions of REST API traits used for both REST clients and servers.
  * Injects [[GenCodec]] and [[GenKeyCodec]] based serialization.
  */
abstract class RestApiCompanion[Real](
  implicit instances: RawRest.FullInstances[Real, DefaultRestImplicits]
) extends RawRest.FullApiCompanion[Real, DefaultRestImplicits] with DefaultRestImplicits
