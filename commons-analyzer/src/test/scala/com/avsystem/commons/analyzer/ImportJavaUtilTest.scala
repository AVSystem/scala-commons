package com.avsystem.commons
package analyzer

import org.scalatest.FunSuite

/**
 * Author: ghik
 * Created: 08/09/15.
 */
class ImportJavaUtilTest extends FunSuite with AnalyzerTest {
  test("import java.util should be rejected") {
    assertErrors(
      """
        |import java.util
        |
        |object whatever
      """.stripMargin
    )
  }
}
