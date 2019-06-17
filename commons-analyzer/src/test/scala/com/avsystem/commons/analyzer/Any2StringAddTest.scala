package com.avsystem.commons
package analyzer

import org.scalatest.FunSuite

class Any2StringAddTest extends FunSuite with AnalyzerTest {
  test("any2stringadd should be rejected") {
    assertErrors(
      """
        |object whatever {
        |  whatever + "fag"
        |}
      """.stripMargin
    )
  }

  test("toString should not be rejected") {
    assertNoErrors(
      """
        |object whatever {
        |  whatever.toString + "fag"
        |}
      """.stripMargin
    )
  }

  test("string interpolation should not be rejected") {
    assertNoErrors(
      """
        |object whatever {
        |  s"${whatever}fag"
        |}
      """.stripMargin
    )
  }
}
