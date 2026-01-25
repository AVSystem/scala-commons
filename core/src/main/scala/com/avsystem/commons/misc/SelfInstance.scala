package com.avsystem.commons
package misc

case class SelfInstance[C[_]](instance: C[Any])
object SelfInstance extends SelfInstanceMacros
