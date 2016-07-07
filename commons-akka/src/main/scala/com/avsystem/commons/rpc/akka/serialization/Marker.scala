package com.avsystem.commons
package rpc.akka.serialization

import com.avsystem.commons.misc.{Bidirectional, Opt, SealedEnumCompanion}
import PrimitiveSizes._

/**
  * @author Wojciech Milewski
  */
private sealed abstract class Marker(val byte: Byte)
private sealed abstract class CompileTimeSize(val size: Int, byte: Byte) extends Marker(byte)
private sealed abstract class RuntimeSize(byte: Byte) extends Marker(byte)
private sealed trait BiMarker {
  def byte: Byte
}


private case object NullMarker extends CompileTimeSize(0, 0)
private case object StringMarker extends RuntimeSize(1)
private case object ByteMarker extends CompileTimeSize(ByteBytes, 2)
private case object ShortMarker extends CompileTimeSize(ShortBytes, 3)
private case object IntMarker extends CompileTimeSize(IntBytes, 4)
private case object LongMarker extends CompileTimeSize(LongBytes, 5)
private case object FloatMarker extends CompileTimeSize(FloatBytes, 6)
private case object DoubleMarker extends CompileTimeSize(DoubleBytes, 7)
private case object ByteArrayMarker extends RuntimeSize(8)
private case object BooleanMarker extends CompileTimeSize(ByteBytes, 9)
private case object ListStartMarker extends RuntimeSize(10) with BiMarker
private case object ObjectStartMarker extends RuntimeSize(11) with BiMarker
private case object ListEndMarker extends Marker(12)
private case object ObjectEndMarker extends Marker(13)

private object Marker extends SealedEnumCompanion[Marker] {
  val values: List[Marker] = caseObjects

  private val markers = values.toArray.sortBy(_.byte)

  def of(byte: Byte): Opt[Marker] =
    if (byte >= 0 && byte < markers.length) Opt(markers(byte)) else Opt.empty
}