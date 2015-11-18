package com.avsystem.commons
package tuples

import scala.language.higherKinds

trait TypeAppliers[T[_]] {
  def simpleApply[A](a: A): T[A]

  trait TypeApply[In, Out] {
    def apply(in: In): Out
  }

}