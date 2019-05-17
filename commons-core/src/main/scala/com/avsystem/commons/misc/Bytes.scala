package com.avsystem.commons
package misc

import java.nio.charset.StandardCharsets

import com.avsystem.commons.serialization.GenCodec

/**
  * General purpose wrapper over byte array which adds `equals`, `hashCode` and `toString` which
  * work on array elements instead of object identity.
  */
case class Bytes(bytes: Array[Byte]) {
  override def hashCode(): Int = java.util.Arrays.hashCode(bytes)
  override def equals(obj: Any): Boolean = obj match {
    case Bytes(obytes) => java.util.Arrays.equals(bytes, obytes)
    case _ => false
  }
  override def toString: String =
    bytes.iterator.map(b => f"${b & 0xFF}%02X").mkString
}
object Bytes {
  def apply(str: String): Bytes = Bytes(str.getBytes(StandardCharsets.UTF_8))
  def parse(hex: String): Bytes = Bytes(hex.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray)

  implicit val codec: GenCodec[Bytes] =
    GenCodec.nullableSimple(i => Bytes(i.readBinary()), (o, b) => o.writeBinary(b.bytes))
}
