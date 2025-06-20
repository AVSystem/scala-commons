package com.avsystem.commons
package opaque

/** A class that provides evidence that values of type A can be safely cast to type B and vice versa.
  * This is used internally by the opaque types implementation to enable safe casting between
  * the underlying type and the opaque type.
  */
private[opaque] final class Castable[A, B]

private[opaque] object Castable {
  type <:>[A, B] = Castable[A, B]

  /** Retrieves an implicit Castable instance
    *
    * @param ev The implicit evidence that A can be cast to B
    * @return The Castable instance
    */
  def apply[A, B](implicit ev: A <:> B): Castable[A, B] = ev

  /** A trait providing operations for casting between types using Castable evidence */
  private[opaque] trait Ops {
    /** Wraps a value of type From as a value of type To
      *
      * @param value The value to wrap
      * @param ev    Evidence that From can be cast to To
      * @return The value as type To
      */
    @inline final def wrap[From, To](value: From)(implicit ev: From <:> To): To = value.asInstanceOf[To]

    /** Unwraps a value of type To to a value of type From
      *
      * @param value The value to unwrap
      * @param ev    Evidence that From can be cast to To
      * @return The value as type From
      */
    @inline final def unwrap[From, To](value: To)(implicit ev: From <:> To): From = value.asInstanceOf[From]

    /** Wraps a container F of values of type From as a container F of values of type To
      *
      * @param value The container to wrap
      * @param ev    Evidence that From can be cast to To
      * @return The container with values as type To
      */
    @inline final def wrapF[From, To, F[_]](value: F[From])(implicit ev: From <:> To): F[To] = value.asInstanceOf[F[To]]

    /** Unwraps a container F of values of type To to a container F of values of type From
      *
      * @param value The container to unwrap
      * @param ev    Evidence that From can be cast to To
      * @return The container with values as type From
      */
    @inline final def unwrapF[From, To, F[_]](value: F[To])(implicit ev: From <:> To): F[From] = value.asInstanceOf[F[From]]
  }
}
