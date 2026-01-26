package com.avsystem.commons.annotation

import scala.quoted.*

trait PositionedMacros {
  inline def here: Int = ${ PositionedMacros.hereImpl }
}

object PositionedMacros {
  def hereImpl(using Quotes): Expr[Int] = '{ ??? }
}
