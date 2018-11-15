package com.avsystem.commons
package misc

case class SelfInstance[C[_]](instance: C[_])
object SelfInstance {
  implicit def materialize[C[_]]: SelfInstance[C] = macro macros.misc.MiscMacros.selfInstance[C[_]]
}
