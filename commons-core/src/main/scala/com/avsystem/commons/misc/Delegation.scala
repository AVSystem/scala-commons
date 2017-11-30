package com.avsystem.commons
package misc

/**
  * A typeclass which witnesses that type `A` can be wrapped into trait or abstract class `B`
  */
trait Delegation[A, B] {
  def delegate(a: A): B
}

object Delegation {
  implicit def materializeDelegation[A, B]: Delegation[A, B] =
  macro com.avsystem.commons.macros.misc.DelegationMacros.materializeDelegation[A, B]

  /**
    * Provides following syntax:
    *
    * Delegation[TargetType](value)
    *
    */
  def apply[B] = new CurriedDelegation[B]

  class CurriedDelegation[B] {
    def apply[A](source: A): B = macro com.avsystem.commons.macros.misc.DelegationMacros.delegate[A, B]
  }
}
