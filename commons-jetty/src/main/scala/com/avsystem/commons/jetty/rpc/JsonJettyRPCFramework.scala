package com.avsystem.commons
package jetty.rpc

import com.avsystem.commons.serialization.json.{JsonStringInput, JsonStringOutput}

object JsonJettyRPCFramework extends JettyRPCFramework {
  type ParamTypeMetadata[T] = ClassTag[T]
  type ResultTypeMetadata[T] = DummyImplicit

  def read[T: Reader](raw: RawValue): T = JsonStringInput.read[T](raw)
  def write[T: Writer](value: T): RawValue = JsonStringOutput.write[T](value)
}
