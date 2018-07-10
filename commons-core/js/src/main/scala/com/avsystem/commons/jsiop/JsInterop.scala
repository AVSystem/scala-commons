package com.avsystem.commons
package jsiop

import com.avsystem.commons.misc.TimestampConversions

import scala.scalajs.js

trait JsInterop {
  implicit def jsDateTimestampConversions(jsDate: js.Date): TimestampConversions =
    new TimestampConversions(jsDate.getTime.toLong)
}
