package com.avsystem.commons
package rpc.akka.serialization

import com.avsystem.commons.misc.Bidirectional

/**
  * @author Wojciech Milewski
  */
private[serialization] sealed trait Marker {
  def byte: Byte = Marker.markerToByte(this)
}

private[serialization] object Marker {
  private val (markerToByte, byteToMarker) = Bidirectional[Marker, Byte] {
    case NullMarker => 0
    case StringMarker => 1
    case ByteMarker => 2
    case ShortMarker => 3
    case IntMarker => 4
    case LongMarker => 5
    case FloatMarker => 6
    case DoubleMarker => 7
    case ByteArrayMarker => 8
    case BooleanMarker => 9
    case ListStartMarker => 10
    case ObjectStartMarker => 12
  }

  def of(byte: Byte): Option[Marker] = byteToMarker.lift(byte)
}

private[serialization] sealed trait CompileTimeSize extends Marker {
  def size: Int = CompileTimeSize.size(this)
}

private[serialization] object CompileTimeSize {

  import PrimitiveSizes._

  private def size(marker: CompileTimeSize): Int = marker match {
    case NullMarker => 0
    case ByteMarker => ByteBytes
    case ShortMarker => ShortBytes
    case IntMarker => IntBytes
    case LongMarker => LongBytes
    case FloatMarker => FloatBytes
    case DoubleMarker => DoubleBytes
    case BooleanMarker => ByteBytes
  }
}

private[serialization] sealed trait RuntimeSize extends Marker

private[serialization] case object NullMarker extends Marker with CompileTimeSize
private[serialization] case object StringMarker extends Marker with RuntimeSize
private[serialization] case object ByteMarker extends Marker with CompileTimeSize
private[serialization] case object ShortMarker extends Marker with CompileTimeSize
private[serialization] case object IntMarker extends Marker with CompileTimeSize
private[serialization] case object LongMarker extends Marker with CompileTimeSize
private[serialization] case object FloatMarker extends Marker with CompileTimeSize
private[serialization] case object DoubleMarker extends Marker with CompileTimeSize
private[serialization] case object ByteArrayMarker extends Marker with RuntimeSize
private[serialization] case object BooleanMarker extends Marker with CompileTimeSize
private[serialization] case object ListStartMarker extends Marker with RuntimeSize
private[serialization] case object ObjectStartMarker extends Marker with RuntimeSize