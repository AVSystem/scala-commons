package com.avsystem.commons
package misc

/**
  * Author: ghik
  * Created: 11/12/15.
  */
object SealedUtils {
  def caseObjectsFor[T]: List[T] = macro macros.misc.SealedMacros.caseObjectsFor[T]
}
