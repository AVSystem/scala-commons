package com.avsystem.commons
package misc

/** A typeclass which witnesses that type `A` can be wrapped into trait or abstract class `B`
  *
  * @deprecated
  *   `Delegation` is being removed in the Scala 3 release of scala-commons. The blackbox macros have no Scala 3
  *   counterpart; write the delegating wrapper manually
  *   (`new TargetTrait { def x = source.x; ... }`).
  */
@deprecated(
  "Delegation will be removed in the Scala 3 release of scala-commons. Write the delegating wrapper manually.",
  since = "2.29.0",
)
trait Delegation[A, B] {
  def delegate(a: A): B
}

@deprecated(
  "Delegation will be removed in the Scala 3 release of scala-commons. Write the delegating wrapper manually.",
  since = "2.29.0",
)
object Delegation {
  implicit def materializeDelegation[A, B]: Delegation[A, B] =
    macro com.avsystem.commons.macros.misc.DelegationMacros.materializeDelegation[A, B]

  /** Provides following syntax:
    *
    * Delegation[TargetType](value)
    */
  def apply[B] = new CurriedDelegation[B]

  class CurriedDelegation[B] {
    def apply[A](source: A): B = macro com.avsystem.commons.macros.misc.DelegationMacros.delegate[A, B]
  }
}
