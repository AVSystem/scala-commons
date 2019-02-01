package com.avsystem.commons
package misc

import com.avsystem.commons.meta.MacroInstances
import com.avsystem.commons.serialization.GenCodec

case class A(str: String)
case class B(int: Int)

object ACodec {
  implicit val aCodec: GenCodec[A] = GenCodec.materialize
}
object BCodec {
  implicit val bCodec: GenCodec[B] = GenCodec.materialize
}

abstract class HasGenCodecUsingAB[T](
  implicit instances: MacroInstances[(ACodec.type, BCodec.type), () => GenCodec[T]]
) {
  implicit lazy val codec: GenCodec[T] = instances((ACodec, BCodec), this).apply()
}

case class AwithB(a: A, b: B)
object AwithB extends HasGenCodecUsingAB[AwithB]
