package com.avsystem.commons
package analyzer

import org.scalatest.funsuite.AnyFunSuite

final class ExplicitGenericsTest extends AnyFunSuite with AnalyzerTest:

  test("inferred generic should be rejected") {
    assertErrors(
      """
        |import com.avsystem.commons.analyzer.TestUtils
        |
        |object whatever {
        |  val x = TestUtils.genericMethod(123)
        |}
      """.stripMargin
    )
  }

  test("inferred generic in macro should be rejected") {
    assertErrors(
      """
        |import com.avsystem.commons.analyzer.TestUtils
        |
        |object whatever {
        |  val x = TestUtils.genericMacro(123)
        |}
      """.stripMargin
    )
  }

  test("explicit generic should not be rejected") {
    assertNoErrors(
      """
        |import com.avsystem.commons.analyzer.TestUtils
        |
        |object whatever {
        |  val x = TestUtils.genericMethod[Int](123)
        |}
      """.stripMargin
    )
  }

  test("explicit generic in macro should not be rejected") {
    assertNoErrors(
      """
        |import com.avsystem.commons.analyzer.TestUtils
        |
        |object whatever {
        |  val x = TestUtils.genericMacro[Int](123)
        |}
      """.stripMargin
    )
  }

end ExplicitGenericsTest
