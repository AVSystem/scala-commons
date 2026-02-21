package com.avsystem.commons
package analyzer

import com.avsystem.commons.annotation.atLeast

object TestUtils {
  def need3Params(@atLeast(3) params: Int*): Unit = ()
}
