package com.avsystem.commons
package analyzer

import com.avsystem.commons.annotation.atLeast

/**
 * Author: ghik
 * Created: 08/09/15.
 */
object TestUtils {
  def need3Params(@atLeast(3) args: Any*) = ()
}
