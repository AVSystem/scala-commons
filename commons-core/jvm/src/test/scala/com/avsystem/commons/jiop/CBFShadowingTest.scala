package com.avsystem.commons
package jiop

import org.scalatest.FunSuite

class CBFShadowingTest extends FunSuite {
  test("java collections CanBuildFroms should not shadow Scala default ones when imported directly") {
    assert(List(1, 2, 3).map(_.toString) == List("1", "2", "3"))
  }

  test("java collections CanBuildFroms should not shadow Scala default ones when imported by wildcard") {
    assert(List(1, 2, 3).map(_.toString) == List("1", "2", "3"))
  }

  test("java collections CanBuildFroms should not shadow Scala default ones when declared explicitly") {
    assert(List(1, 2, 3).map(_.toString) == List("1", "2", "3"))
  }
}
