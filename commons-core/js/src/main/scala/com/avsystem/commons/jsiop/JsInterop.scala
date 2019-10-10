package com.avsystem.commons
package jsiop

import com.avsystem.commons.TimestampConversions

import scala.scalajs.js

trait JsInterop {
  implicit def jsDateTimestampConversions(jsDate: js.Date): TimestampConversions =
    new TimestampConversions(jsDate.getTime.toLong)
}
object JsInterop extends JsInterop
