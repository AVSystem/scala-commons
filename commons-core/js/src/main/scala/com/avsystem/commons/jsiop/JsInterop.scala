package com.avsystem.commons
package jsiop

import com.avsystem.commons.jsiop.JsInterop.{JsOptOps, UndefOrOps}
import com.avsystem.commons.misc.TimestampConversions

import scala.scalajs.js
import scala.scalajs.js.UndefOr

trait JsInterop {
  implicit def jsDateTimestampConversions(jsDate: js.Date): TimestampConversions =
    new TimestampConversions(jsDate.getTime.toLong)

  implicit def undefOrOps[A](undefOr: UndefOr[A]): UndefOrOps[A] = new UndefOrOps(undefOr)
  implicit def jsOptOps[A](opt: Opt[A]): JsOptOps[A] = new JsOptOps(opt)
}
object JsInterop extends JsInterop {
  class UndefOrOps[A](private val value: UndefOr[A]) extends AnyVal {
    def toOpt: Opt[A] = if (value.isDefined) Opt(value.get) else Opt.Empty
  }

  class JsOptOps[A](private val value: Opt[A]) extends AnyVal {
    def orUndefined: UndefOr[A] = if (value.isDefined) js.defined(value.get) else js.undefined
  }
}
