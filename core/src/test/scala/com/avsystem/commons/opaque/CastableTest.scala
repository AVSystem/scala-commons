package com.avsystem.commons
package opaque

import org.scalatest.funsuite.AnyFunSuiteLike

final class CastableTest extends AnyFunSuiteLike {
  test("cast only works for compatible types") {
    assertCompiles(
      //language=scala
      """
        |new Castable.Ops {
        |      implicit val intToLong: Castable[Int, Long] = new Castable[Int, Long]
        |
        |      wrap[Int, Long](42)
        |      unwrap[Int, Long](42L)
        |      wrapF[Int, Long, List](List(42))
        |      unwrapF[Int, Long, List](List(42L))
        |
        |      wrap(42)
        |      unwrap(42L)
        |      wrapF(List(42))
        |      unwrapF(List(42L))
        |    }
        |""".stripMargin,
    )
    assertDoesNotCompile(
      //language=scala
      """
        |new Castable.Ops {
        |      wrap[Int, Long](42)
        |      unwrap[Int, Long](42L)
        |      wrapF[Int, Long, List](List(42))
        |      unwrapF[Int, Long, List](List(42L))
        |
        |      wrap(42)
        |      unwrap(42L)
        |      wrapF(List(42))
        |      unwrapF(List(42L))
        |    }
        |""".stripMargin,
    )
  }
}
