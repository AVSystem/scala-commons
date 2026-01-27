package com.avsystem.commons
package misc

object ValueOf {
  def apply[T](using vof: ValueOf[T]): T = vof.value
}
