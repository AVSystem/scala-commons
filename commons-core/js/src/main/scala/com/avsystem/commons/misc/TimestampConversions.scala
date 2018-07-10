package com.avsystem.commons
package misc

import scala.scalajs.js

final class TimestampConversions(private val millis: Long) extends AnyVal {
  def toJsDate: js.Date = new js.Date(millis.toDouble)
  def toJDate: JDate = new JDate(millis)
}
