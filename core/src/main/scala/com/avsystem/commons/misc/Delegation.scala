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

  inline implicit def materializeDelegation[A, B]: Delegation[A, B] = ${ materializeDelegationImpl[A, B] }
  def materializeDelegationImpl[A: Type, B: Type](using Quotes): Expr[Delegation[A, B]] = '{ ??? }

  /**
   * Provides following syntax:
   *
   * Delegation[TargetType](value)
   */
  def apply[B] = new CurriedDelegation[B]

  class CurriedDelegation[B] {
    def apply[A](source: A): B = macro com.avsystem.commons.macros.misc.DelegationMacros.delegate[A, B]
    inline def apply[A](source: A): B = ${ delegateImpl[A, B]('source) }
  }
}

def delegateImpl[A: Type, B: Type](source: Expr[A])(using Quotes): Expr[B] = '{ ??? }
