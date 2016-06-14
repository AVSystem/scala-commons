package com.avsystem.commons
package rpc.akka.serialization

import java.{lang => jl}

/**
  * @author Wojciech Milewski
  */
private[serialization] object PrimitiveSizes {
  val ByteBytes = jl.Byte.BYTES
  val ShortBytes = jl.Short.BYTES
  val IntBytes = jl.Integer.BYTES
  val LongBytes = jl.Long.BYTES
  val FloatBytes = jl.Float.BYTES
  val DoubleBytes = jl.Double.BYTES
}
