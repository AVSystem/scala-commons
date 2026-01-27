package com.avsystem.commons
package jsiop

import com.avsystem.commons.misc.TimestampConversions

import scala.scalajs.js
import scala.scalajs.js.UndefOr

trait JsInterop {
  given Conversion[js.Date, TimestampConversions] =
     TimestampConversions(_.getTime().toLong)

  extension[A](undefOr: UndefOr[A]) {
    def toOpt: Opt[A] = if (undefOr.isDefined) Opt(undefOr.get) else Opt.Empty
  }

  extension[A](raw: A) {
    def orUndefined: UndefOr[A] = if (Opt(raw).isDefined) js.defined(raw) else js.undefined
  }
}
object JsInterop extends JsInterop 
