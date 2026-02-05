package com.avsystem.commons
package serialization

import org.scalatest.funsuite.AnyFunSuite

class WhenAbsentTest extends AnyFunSuite {
  test("whenAbsent.value macro") {
    case class Test(@whenAbsent(42) foo: Int = whenAbsent.value)
    val t = Test()
    assert(t.foo == 42)
  }

  test("whenAbsent.value macro with complex expression") {
    case class Test(@whenAbsent(List(1, 2, 3)) foo: List[Int] = whenAbsent.value)
    val t = Test()
    assert(t.foo == List(1, 2, 3))
  }

  test("whenAbsent.value macro with throw") {
    case class Test(@whenAbsent(throw new Exception("No value")) foo: Int = whenAbsent.value)
    val thrown = intercept[Exception] {
      Test()
    }
    assert(thrown.getMessage == "No value")
  }

  test("whenAbsent.value macro for method") {
    object Test{
      def method(@whenAbsent(100) foo: Int = whenAbsent.value): Int = foo
    }
    assert(Test.method() == 100)
  }
}
