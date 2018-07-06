package com.avsystem.commons
package rest

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
