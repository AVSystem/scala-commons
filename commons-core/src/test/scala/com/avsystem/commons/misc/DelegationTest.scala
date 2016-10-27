package com.avsystem.commons
package misc

import org.scalatest.FunSuite

import scala.language.higherKinds

class DelegationTest extends FunSuite {
  trait Destination[T] {
    val te: T
    def simple(omg: Int): String
    def meth[C[+X] >: Null <: Traversable[X]](map: Map[T, C[String]]): C[(T, String)]
  }

  class Source {
    val te: Double = 3.14
    def simple(omg: Int): String = omg.toString
    def meth[C[+X] >: Null <: Traversable[X]](map: Map[Double, C[String]]): C[(Double, String)] = null
  }

  test("simple test") {
    val source = new Source
    val destination = Delegation[Destination[Double]](source)

    assert(source.te == destination.te)
    assert(source.simple(42) == destination.simple(42))
    assert(source.meth[List](Map.empty) == destination.meth[List](Map.empty))
  }
}
