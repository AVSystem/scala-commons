package com.avsystem.commons
package rpc.akka.serialization

import com.avsystem.commons.misc.{Opt, SealedEnumCompanion}

/**
  * @author Wojciech Milewski
  */
private sealed abstract class Marker(val byte: Byte)
private sealed abstract class StaticSize(val size: Int, byte: Byte) extends Marker(byte)
private sealed abstract class DynamicSize(byte: Byte) extends Marker(byte)
private sealed trait BiMarker {
  def byte: Byte
}


private case object NullMarker extends StaticSize(0, 0)
private case object StringMarker extends DynamicSize(1)
private case object ByteMarker extends StaticSize(ByteBytes, 2)
private case object ShortMarker extends StaticSize(ShortBytes, 3)
private case object IntMarker extends StaticSize(IntBytes, 4)
private case object LongMarker extends StaticSize(LongBytes, 5)
private case object FloatMarker extends StaticSize(FloatBytes, 6)
private case object DoubleMarker extends StaticSize(DoubleBytes, 7)
private case object ByteArrayMarker extends DynamicSize(8)
private case object BooleanMarker extends StaticSize(ByteBytes, 9)
private case object ListStartMarker extends DynamicSize(10) with BiMarker
private case object ObjectStartMarker extends DynamicSize(11) with BiMarker
private case object ListEndMarker extends Marker(12)
private case object ObjectEndMarker extends Marker(13)

private object Marker extends SealedEnumCompanion[Marker] {
  val values: List[Marker] = caseObjects

  private val markers = values.toArray.sortBy(_.byte)

  def of(byte: Byte): Opt[Marker] =
    if (byte >= 0 && byte < markers.length) Opt(markers(byte)) else Opt.empty
}