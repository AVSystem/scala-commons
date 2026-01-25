package com.avsystem.commons
package misc

/**
 * A typeclass which witnesses that type `A` can be wrapped into trait or abstract class `B`
 */
trait Delegation[A, B] {
  def delegate(a: A): B
}

object Delegation extends DelegationMacros {
  /**
   * Provides following syntax:
   *
   * Delegation[TargetType](value)
   */
  def apply[B] = new CurriedDelegation[B]

  class CurriedDelegation[B] extends DelegationApplyMacros[B]
}
