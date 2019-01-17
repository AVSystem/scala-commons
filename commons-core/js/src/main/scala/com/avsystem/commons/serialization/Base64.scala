package com.avsystem.commons
package serialization

object Base64 {
  private def fail =
    throw new UnsupportedOperationException("Base64 encoding is currently not supported in ScalaJS")

  def encode(bytes: Array[Byte], withoutPadding: Boolean = false, urlSafe: Boolean = false): String = fail
  def decode(base64: String, urlSafe: Boolean = false): Array[Byte] = fail
}
