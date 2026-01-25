package com.avsystem.commons
package annotation

trait PositionedMacros {
  def here: Int = macro com.avsystem.commons.macros.misc.MiscMacros.posPoint
}
