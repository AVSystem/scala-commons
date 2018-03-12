package com.avsystem.commons
package jetty.rpc

import com.avsystem.commons.serialization.GenCodec
import upickle.Js

object JsonJettyRPCFramework extends JettyRPCFramework {
  type RawValue = Js.Value
  type Reader[T] = GenCodec[T]
  type Writer[T] = GenCodec[T]
  type ParamTypeMetadata[T] = ClassTag[T]
  type ResultTypeMetadata[T] = DummyImplicit

  def read[T: Reader](raw: RawValue): T = GenCodec.read[T](new JsValueInput(raw))
  def write[T: Writer](value: T): RawValue = JsValueOutput.write[T](value)

  override protected def valueToJson(value: RawValue): String = upickle.json.write(value)
  override protected def jsonToValue(json: String): Js.Value = upickle.json.read(json)
  override protected def argsToJson(argLists: List[List[RawValue]]): String =
    upickle.json.write(Js.Arr(argLists.map { args => Js.Arr(args: _*) }: _*))
  override protected def jsonToArgs(json: String): List[List[Js.Value]] = {
    upickle.json.read(json) match {
      case Js.Arr(lists@_*) =>
        lists.collect {
          case Js.Arr(args@_*) => args.toList
        }(scala.collection.breakOut)
      case _ => Nil
    }
  }
}
