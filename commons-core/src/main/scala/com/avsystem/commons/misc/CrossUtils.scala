package com.avsystem.commons
package misc

object CrossUtils {
  def cross[T](forJvm: T, forJs: T): T = macro macros.misc.MiscMacros.crossImpl
}
