package com.avsystem.commons
package opaque

import com.avsystem.commons.opaque.Subopaque.*
import com.avsystem.commons.opaque.SubopaqueTest.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class SubopaqueTest extends AnyFlatSpec with Matchers {

  "SubOpaque" should "create a type with no runtime overhead" in {
    PosInt(1) shouldEqual 1
    PosInt(-1) shouldEqual 0
  }

  it should "be a subtype of its Repr" in {
    type Foo = Foo.Type
    object Foo extends Subopaque.Default[Int]
    assertCompiles("Foo(1): Foo")
    assertCompiles("Foo(1): Int")

    Foo(1) - Foo(0) shouldEqual Foo(1)
  }

  it should "support user ops" in {
    (PosInt(3) -- PosInt(1)) shouldEqual PosInt(2)
  }

  it should "work in Arrays" in {
    object Foo extends Subopaque.Default[Int]

    val foo = Foo(42)
    Array(foo).apply(0) shouldEqual foo
  }

  "Subopaque.Default" should "automatically create an apply method" in {
    object PersonId extends Subopaque.Default[Int]
    PersonId(1) shouldEqual 1
  }
}

object SubopaqueTest {
  object PosInt extends Subopaque[Int] {
    def apply(value: Int): Type = wrap {
      if (value < 0) 0 else value
    }

    implicit final class Ops(private val me: PosInt.Type) extends AnyVal {
      def --(other: PosInt.Type): PosInt.Type = wrap {
        val result = unwrap(me) - unwrap(other)
        if (result < 0) 0 else result
      }
    }
  }
}