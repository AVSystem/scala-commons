package com.avsystem.commons
package macros

trait TypeClassDerivationTestMacros[TC[_]] {
  def materialize[T]: TC[T] = macro macros.TestMacros.materialize[T]

  def typeRepr[T: ru.WeakTypeTag]: String = ru.weakTypeOf[T].toString
}
