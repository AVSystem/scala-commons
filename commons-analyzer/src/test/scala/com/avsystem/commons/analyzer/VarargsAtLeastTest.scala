package com.avsystem.commons
package analyzer

import org.scalatest.FunSuite

/**
  * Author: ghik
  * Created: 08/09/15.
  */
class VarargsAtLeastTest extends FunSuite with AnalyzerTest {
  test("too few varargs parameters should be rejected") {
    assertErrors(
      """
        |import com.avsystem.commons.analyzer.TestUtils
        |
        |object whatever {
        |  TestUtils.need3Params(1, 2)
        |}
      """.stripMargin
    )
  }

  test("enough varargs parameters should not be rejected") {
    assertNoErrors(
      """
        |import com.avsystem.commons.analyzer.TestUtils
        |
        |object whatever {
        |  TestUtils.need3Params(1, 2, 3)
        |  TestUtils.need3Params(1, 2, 3, 4)
        |}
      """.stripMargin
    )
  }

  test("collection passed as varargs parameter should not be rejected") {
    assertNoErrors(
      """
        |import com.avsystem.commons.analyzer.TestUtils
        |
        |object whatever {
        |  TestUtils.need3Params(List(1,2): _*)
        |}
      """.stripMargin
    )
  }
}
