package com.avsystem.commons
package analyzer

import org.scalatest.funsuite.AnyFunSuite

class FinalValueClassesTest extends AnyFunSuite with AnalyzerTest {
  test("value classes should be marked as final") {
    assertErrors(2,
      """
        |object whatever {
        |  // This should pass - final value class
        |  final class GoodValueClass(val x: Int) extends AnyVal {
        |    def double: Int = x * 2
        |  }
        |
        |  // This should fail - value class not marked as final
        |  class BadValueClass1(val x: Int) extends AnyVal {
        |    def double: Int = x * 2
        |  }
        |
        |  // This should fail - another value class not marked as final
        |  class BadValueClass2[T <: Int](val x: T) extends AnyVal {
        |    def double: Int = x * 2
        |  }
        |
        |  // Regular class extending AnyVal but not marked as final - should not be affected
        |  // because it has multiple parameters
        |  class RegularClass(val x: Int, val y: Int) {
        |    def double: Int = x * 2
        |  }
        |
        |  // Regular class not extending AnyVal - should not be affected
        |  class RegularClass2(val x: Int) {
        |    def double: Int = x * 2
        |  }
        |}
      """.stripMargin)
  }
}