package com.avsystem.commons
package misc

/** A typeclass which witnesses that type `A` can be wrapped into trait or abstract class `B`
  */
trait Delegation[A, B] {
  def delegate(a: A): B
}

object Delegation {
  // TODO[scala3-port]: materializeDelegation (Scala 2 macro def) (L)
  implicit def materializeDelegation[A, B]: Delegation[A, B] = ???

  /** Provides following syntax:
    *
    * Delegation[TargetType](value)
    */
  def apply[B] = new CurriedDelegation[B]

  class CurriedDelegation[B] {
    // TODO[scala3-port]: CurriedDelegation.apply (Scala 2 macro def) (L)
    def apply[A](source: A): B = ???
  }
}
