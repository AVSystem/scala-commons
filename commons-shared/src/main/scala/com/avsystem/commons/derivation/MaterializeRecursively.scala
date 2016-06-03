package com.avsystem.commons
package derivation

import scala.language.higherKinds

/**
  * Marker type used internally by automatic type class derivation macros.
  * Used to inform the compiler and macro engine that automatic derivation of particular type class is
  * allowed in some context.
  */
sealed trait MaterializeRecursively[TC[_]]
object MaterializeRecursively {
  def apply[TC[_]]: MaterializeRecursively[TC] = null
}
