package com.avsystem.commons
package jetty.rpc

import com.avsystem.commons.serialization.GenCodec
import jawn.ast.{FastRenderer, JArray, JParser, JValue}

object JsonJettyRPCFramework extends JettyRPCFramework {
  type RawValue = JValue
  type Reader[T] = GenCodec[T]
  type Writer[T] = GenCodec[T]
  type ParamTypeMetadata[T] = ClassTag[T]
  type ResultTypeMetadata[T] = DummyImplicit

  def read[T: Reader](raw: RawValue): T = GenCodec.read[T](new JValueInput(raw))
  def write[T: Writer](value: T): RawValue = JValueOutput.write[T](value)

  override protected def valueToJson(value: RawValue): String = FastRenderer.render(value)
  override protected def jsonToValue(json: String): JValue = JParser.parseFromString(json).get
  override protected def argsToJson(argLists: List[List[RawValue]]): String =
    FastRenderer.render(JArray(argLists.map { args => JArray(args.toArray) }(scala.collection.breakOut)))
  override protected def jsonToArgs(json: String): List[List[JValue]] = {
    JParser.parseFromString(json).get match {
      case JArray(lists) =>
        lists.collect {
          case JArray(args) => args.toList
        }(scala.collection.breakOut)
      case _ => Nil
    }
  }
}
