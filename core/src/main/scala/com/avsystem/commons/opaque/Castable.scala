package com.avsystem.commons
package opaque

private[opaque] final class Castable[A, B]
private[opaque] object Castable {
  type <:>[A, B] = Castable[A, B]
  def apply[A, B](implicit ev: A <:> B): Castable[A, B] = ev

  private[opaque] trait Ops {
    @inline final def wrap[From, To](value: From)(implicit ev: From <:> To): To = value.asInstanceOf[To]
    @inline final def unwrap[From, To](value: To)(implicit ev: From <:> To): From = value.asInstanceOf[From]
    @inline final def wrapF[From, To, F[_]](value: F[From])(implicit ev: From <:> To): F[To] = value.asInstanceOf[F[To]]
    @inline final def unwrapF[From, To, F[_]](value: F[To])(implicit ev: From <:> To): F[From] = value.asInstanceOf[F[From]]
  }
}
