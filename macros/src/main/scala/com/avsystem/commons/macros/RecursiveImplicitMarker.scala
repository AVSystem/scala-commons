package com.avsystem.commons
package macros

import scala.annotation.compileTimeOnly

object RecursiveImplicitMarker {
  @compileTimeOnly("this can only be used by derivation macros")
  implicit def mark[T]: T = throw new NotImplementedError
}
