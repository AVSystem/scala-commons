package com.avsystem.commons
package serialization

import com.avsystem.commons.serialization.GenCodec.ReadFailure

import scala.scalajs.js

object IsoInstant {
  def format(millis: Long): String =
    new js.Date(millis.toDouble).toISOString

  def parse(string: String): Long = {
    val parsed = js.Date.parse(string)
    if (parsed.isNaN || new js.Date(parsed).toISOString != string) {
      throw new ReadFailure(s"invalid ISO instant: $string")
    }
    parsed.toLong
  }
}
