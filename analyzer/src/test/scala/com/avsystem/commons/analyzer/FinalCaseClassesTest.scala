package com.avsystem.commons
package analyzer

import org.scalatest.funsuite.AnyFunSuite

class FinalCaseClassesTest extends AnyFunSuite with AnalyzerTest {
  test("case classes should be marked as final") {
    assertErrors(2,
      //language=scala
      """
        |object whatever {
        |  // This should pass - final case class
        |  final case class GoodCaseClass(x: Int, y: String) {
        |    def double: Int = x * 2
        |  }
        |
        |  // This should fail - case class not marked as final
        |  case class BadCaseClass1(x: Int, y: String) {
        |    def double: Int = x * 2
        |  }
        |
        |  // This should fail - another case class not marked as final
        |  case class BadCaseClass2[T](x: T, y: String) {
        |    def double: String = y * 2
        |  }
        |
        |  // Regular class - should not be affected
        |  class RegularClass(val x: Int, val y: String) {
        |    def double: Int = x * 2
        |  }
        |
        |  // Regular class with case-like constructor - should not be affected
        |  class RegularClass2(x: Int, y: String) {
        |    def double: Int = x * 2
        |  }
        |}
      """.stripMargin)
  }
}