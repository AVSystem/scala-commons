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

  // SI-4440 https://github.com/scala/bug/issues/4440
  test("should not be affected due to SI-4440") {
    assertNoErrors(
      //language=scala
      """
        |trait Outer {
        |  case class Inner(x: Int, y: String) {
        |    def double: Int = x * 2
        |  }
        |}
        |
        |class Outer2 {
        |  case class Inner(x: Int, y: String) {
        |    def double: Int = x * 2
        |  }
        |}
        """.stripMargin
    )
  }

  test("sealed case class should not be affected") {
    assertNoErrors(
      //language=scala
      """
        |sealed case class SealedCaseClass(x: Int) {
        |  def double: Int = x * 2
        |}
        |object SealedCaseClass {
        |  val jeden = SealedCaseClass(1)
        |  val dwa = SealedCaseClass(2)
        |}
  """.stripMargin
    )
  }
}