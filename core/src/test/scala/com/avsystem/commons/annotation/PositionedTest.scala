package com.avsystem.commons
package annotation

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class PositionedTest extends AnyFunSuite with Matchers {
  val point: Int = positioned.here

  test("positioned.here yields distinct positive offsets at distinct call sites") {
    val a = positioned.here
    val b = positioned.here
    assert(a > 0)
    assert(b > 0)
    assert(a != b)
  }

  test("positioned.here offset is the start offset of the `positioned.here` term") {
    point shouldBe 214
  }
}
