package com.avsystem.commons
package analyzer

import org.scalatest.funsuite.AnyFunSuite

final class Any2StringAddTest extends AnyFunSuite with AnalyzerTest:

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

end Any2StringAddTest
