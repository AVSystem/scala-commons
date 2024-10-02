package com.avsystem.commons
package analyzer

import org.scalatest.funsuite.AnyFunSuite

final class ImportJavaUtilTest extends AnyFunSuite with AnalyzerTest:

  test("import java.util should be rejected") {
    assertErrors(
      """
        |import java.util
        |
        |object whatever
      """.stripMargin
    )
  }

  test("import java.util as ju should not be rejected") {
    assertNoErrors(
      """
        |import java.util as ju
        |
        |object whatever
      """.stripMargin
    )
  }

  test("another import should not be rejected") {
    assertNoErrors(
      """
        |import java.lang
        |
        |object whatever
      """.stripMargin
    )
  }

end ImportJavaUtilTest
