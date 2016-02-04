package com.avsystem.commons
package derivation

import scala.language.higherKinds

/**
  * Author: ghik
  * Created: 04/02/16.
  */
sealed trait AutoDeriveRecursively[TC[_]]
object AutoDeriveRecursively {
  def apply[TC[_]]: AutoDeriveRecursively[TC] = null
}
