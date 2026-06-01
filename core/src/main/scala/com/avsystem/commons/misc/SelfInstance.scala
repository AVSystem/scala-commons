package com.avsystem.commons
package misc

// TODO[scala3-port]: C[_] existential narrowed to C[Any] (Scala 3 forbids HKT wildcard application) (S)
case class SelfInstance[C[_]](instance: C[Any])
object SelfInstance {
  // TODO[scala3-port]: SelfInstance.materialize (Scala 2 macro def) (L)
  given materialize[C[_]]: SelfInstance[C] = ???
}
