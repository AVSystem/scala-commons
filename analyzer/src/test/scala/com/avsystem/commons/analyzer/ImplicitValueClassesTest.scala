package com.avsystem.commons
package analyzer

import org.scalatest.funsuite.AnyFunSuite

class ImplicitValueClassesTest extends AnyFunSuite with AnalyzerTest {
  test("implicit classes should extend AnyVal") {
    assertErrors(2,
      """
        |object whatever {
        |  // This should pass - implicit class extending AnyVal
        |  implicit class GoodImplicitClass(val x: Int) extends AnyVal {
        |    def double: Int = x * 2
        |  }
        |
        |  // This should fail - implicit class not extending AnyVal
        |  implicit class BadImplicitClass1(val x: Int) {
        |    def double: Int = x * 2
        |  }
        |
        |  // This should fail - another implicit class not extending AnyVal
        |  implicit class BadImplicitClass2[T <: Int](val x: T) {
        |    def double: Int = x * 2
        |  }
        |
        |  // Regular class - should not be affected
        |  class RegularClass(val x: Int) {
        |    def double: Int = x * 2
        |  }
        |
        |  // implicit class with implicit parameter - should not be affected
        |  implicit class ImplicitClassWithImplicitParameter(val x: Int)(implicit dummy: DummyImplicit) {
        |    def double: Int = x * 2
        |  }
        |}
      """.stripMargin)
  }
}