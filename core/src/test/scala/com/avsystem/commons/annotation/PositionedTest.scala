package com.avsystem.commons.annotation

import org.scalatest.funsuite.AnyFunSuite

class PositionedTest extends AnyFunSuite {
  test("positioned.here yields distinct positive offsets at distinct call sites") {
    val a = positioned.here
    val b = positioned.here
    assert(a > 0)
    assert(b > 0)
    assert(a != b)
  }
}
