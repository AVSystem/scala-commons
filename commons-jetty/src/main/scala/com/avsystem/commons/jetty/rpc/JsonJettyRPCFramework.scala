package com.avsystem.commons
package jetty.rpc

import com.avsystem.commons.serialization.GenCodec
import com.avsystem.commons.serialization.json.{JsonStringInput, JsonStringOutput}

object JsonJettyRPCFramework extends JettyRPCFramework {
  type RawValue = String
  type Reader[T] = GenCodec[T]
  type Writer[T] = GenCodec[T]
  type ParamTypeMetadata[T] = ClassTag[T]
  type ResultTypeMetadata[T] = DummyImplicit

  def read[T: Reader](raw: RawValue): T = JsonStringInput.read[T](raw)
  def write[T: Writer](value: T): RawValue = JsonStringOutput.write[T](value)

  override protected def valueToJson(value: RawValue): String = value
  override protected def jsonToValue(json: String): String = json
  override protected def argsToJson(argLists: List[List[RawValue]]): String = write(argLists)
  override protected def jsonToArgs(json: String): List[List[RawValue]] = read[List[List[RawValue]]](json)
}
