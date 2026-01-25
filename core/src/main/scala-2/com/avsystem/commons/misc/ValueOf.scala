package com.avsystem.commons
package misc

import com.avsystem.commons.macros.misc.MiscMacros

trait ValueOfMacros {
  implicit def mkValueOf[T]: ValueOf[T] = macro MiscMacros.mkValueOf[T]
}
