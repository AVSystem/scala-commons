package com.avsystem.commons
package misc

import com.avsystem.commons.serialization.{GenCodec, GenKeyCodec, transparent}

/**
  * Typeclass that contains string representation of a concrete type. This representation should correctly parse
  * and typecheck when used as a type in Scala source code.
  */
@transparent
case class TypeString[T](value: String) extends AnyVal
object TypeString {
  def of[T](implicit ts: TypeString[T]): String = ts.value

  implicit def materialize[T]: TypeString[T] = macro macros.misc.MiscMacros.typeString[T]

  implicit val keyCodec: GenKeyCodec[TypeString[_]] =
    GenKeyCodec.create[TypeString[Any]](TypeString(_), _.value).asInstanceOf[GenKeyCodec[TypeString[_]]]

  implicit val codec: GenCodec[TypeString[_]] =
    GenCodec.materialize[TypeString[Any]].asInstanceOf[GenCodec[TypeString[_]]]
}
