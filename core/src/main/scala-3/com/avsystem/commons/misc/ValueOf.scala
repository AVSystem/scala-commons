package com.avsystem.commons
package misc

import scala.quoted.*

trait ValueOfMacros {
  inline implicit def mkValueOf[T]: ValueOf[T] = ${ ValueOfMacros.mkValueOfImpl[T] }
}

object ValueOfMacros {
  def mkValueOfImpl[T: Type](using Quotes): Expr[ValueOf[T]] = '{ ??? }.asInstanceOf[Expr[ValueOf[T]]]
}
