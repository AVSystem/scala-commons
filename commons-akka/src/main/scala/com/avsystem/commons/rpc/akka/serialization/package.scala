package com.avsystem.commons
package rpc.akka

import java.{lang => jl}

package object serialization {
  val ByteBytes = jl.Byte.BYTES
  val ShortBytes = jl.Short.BYTES
  val IntBytes = jl.Integer.BYTES
  val LongBytes = jl.Long.BYTES
  val FloatBytes = jl.Float.BYTES
  val DoubleBytes = jl.Double.BYTES

  val TrueByte: Byte = 1
  val FalseByte: Byte = 0
}
