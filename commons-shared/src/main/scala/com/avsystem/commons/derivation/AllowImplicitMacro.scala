package com.avsystem.commons
package derivation

/**
  * Marker type used internally by automatic type class derivation macros.
  * Used to inform the compiler and macro engine that automatic derivation of particular type class is
  * allowed in some context.
  */
sealed trait AllowImplicitMacro[T]
object AllowImplicitMacro {
  def apply[T]: AllowImplicitMacro[T] = null
}
