package com.avsystem.commons
package misc

import java.nio.charset.StandardCharsets

import com.avsystem.commons.serialization.Base64

/**
  * General purpose wrapper over byte array which adds `equals`, `hashCode` and `toString` which
  * work on array elements instead of object identity.
  */
final case class Bytes(bytes: Array[Byte]) {
  def hex: String = bytes.iterator.map(b => f"${b & 0xFF}%02X").mkString

  def base64: String = base64()
  def base64(withoutPadding: Boolean = false, urlSafe: Boolean = false): String =
    Base64.encode(bytes, withoutPadding, urlSafe)

  override def hashCode(): Int = java.util.Arrays.hashCode(bytes)
  override def equals(obj: Any): Boolean = obj match {
    case Bytes(obytes) => java.util.Arrays.equals(bytes, obytes)
    case _ => false
  }
  override def toString: String = hex
}
object Bytes {
  def apply(str: String): Bytes = Bytes(str.getBytes(StandardCharsets.UTF_8))
  def fromHex(hex: String): Bytes = Bytes(hex.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray)
  def fromBase64(base64: String, urlSafe: Boolean = false): Bytes = Bytes(Base64.decode(base64, urlSafe))
}
