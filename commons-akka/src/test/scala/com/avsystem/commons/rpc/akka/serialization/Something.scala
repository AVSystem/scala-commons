package com.avsystem.commons
package rpc.akka.serialization

import com.avsystem.commons.serialization.GenCodec

/**
  * @author Wojciech Milewski
  */
case class Something(
  str: String,
  nullable: String,
  boolean: Boolean,
  int: Int,
  list: List[Int],
  nested: Nested
)

object Something {
  implicit val codec: GenCodec[Something] = GenCodec.materialize
}

case class Nested(
  str: String,
  nullable: String,
  boolean: Boolean,
  int: Int,
  list: List[Int]
)

object Nested {
  implicit val codec: GenCodec[Nested] = GenCodec.materialize
}