package com.avsystem.commons
package macros

trait ApplierUnapplier[T, F] {
  def apply(f: F): T
  def unapply(t: T): F
}

/**
  * Author: ghik
  * Created: 02/12/15.
  */
object ApplyUnapplyTest {
  case class Empty()
  case class Single(int: Int)
  case class Multiple(int: Int, str: String)
  case class Gadt[T](t: T, list: List[T], cos: String)

  trait Custom[T]
  object Custom {
    def apply[T](t: T): Custom[T] = null
    def unapply[T](whatever: Custom[T]): Option[T] = None
  }

  def applierUnapplier[T, F]: ApplierUnapplier[T, F] = macro TestMacros.applierUnapplier[T, F]

  applierUnapplier[Empty, Unit]
  applierUnapplier[Single, Int]
  applierUnapplier[Multiple, (Int, String)]
  applierUnapplier[Gadt[Int], (Int, List[Int], String)]
  applierUnapplier[Custom[String], String]
}
