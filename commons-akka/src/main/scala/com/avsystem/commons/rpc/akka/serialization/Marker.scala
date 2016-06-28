package com.avsystem.commons
package rpc.akka.serialization

import com.avsystem.commons.misc.Bidirectional
import PrimitiveSizes._

/**
  * @author Wojciech Milewski
  */
private sealed trait Marker {
  def byte: Byte = Marker.markerToByte(this)
}

private object Marker {
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
    case ObjectStartMarker => 11
  }

  def of(byte: Byte): Option[Marker] = byteToMarker.lift(byte)
}

private sealed abstract class CompileTimeSize(val size: Int) extends Marker
private sealed trait RuntimeSize extends Marker

private case object NullMarker extends CompileTimeSize(0)
private case object ByteMarker extends CompileTimeSize(ByteBytes)
private case object ShortMarker extends CompileTimeSize(ShortBytes)
private case object IntMarker extends CompileTimeSize(IntBytes)
private case object LongMarker extends CompileTimeSize(LongBytes)
private case object FloatMarker extends CompileTimeSize(FloatBytes)
private case object DoubleMarker extends CompileTimeSize(DoubleBytes)
private case object BooleanMarker extends CompileTimeSize(ByteBytes)
private case object StringMarker extends RuntimeSize
private case object ByteArrayMarker extends RuntimeSize
private case object ListStartMarker extends RuntimeSize
private case object ObjectStartMarker extends RuntimeSize