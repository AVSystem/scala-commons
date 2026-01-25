package com.avsystem.commons
package macros

trait TypeClassDerivationTestMacros[TC[_]] {
  def materialize[T]: TC[T] = ???

  def typeRepr[T]: String = ???
}
