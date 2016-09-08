package com.avsystem.commons
package rpc.akka.serialization

import com.avsystem.commons.serialization.GenCodec

/**
  * @author Wojciech Milewski
  */
case class Something(int: Int, nested: Nested, str: String)
case class Nested(list: List[Int], int: Int)

object Nested {
  implicit val codec: GenCodec[Nested] = GenCodec.materialize
}
object Something {
  implicit val codec: GenCodec[Something] = GenCodec.materialize
}