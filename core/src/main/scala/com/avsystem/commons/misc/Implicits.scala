package com.avsystem.commons
package misc

object Implicits {
  // TODO[scala3-port]: infer (Scala 2 macro def) (L)
  def infer[T]: T = ???
  // TODO[scala3-port]: infer(clue) (Scala 2 macro def) (L)
  def infer[T](clue: String): T = ???
  // TODO[scala3-port]: inferNonMacro (Scala 2 macro def) (L)
  def inferNonMacro[T](clue: String): T = ???
}
