package com.avsystem.commons
package serialization

import java.util.{Base64 => JBase64}

object Base64 {
  def encode(bytes: Array[Byte], withoutPadding: Boolean = false, urlSafe: Boolean = false): String = {
    val encoder = (if (urlSafe) JBase64.getUrlEncoder else JBase64.getEncoder) |>
      (e => if (withoutPadding) e.withoutPadding else e)
    encoder.encodeToString(bytes)
  }

  def decode(base64: String, urlSafe: Boolean = false): Array[Byte] = {
    val decoder = if (urlSafe) JBase64.getUrlDecoder else JBase64.getDecoder
    decoder.decode(base64)
  }
}
