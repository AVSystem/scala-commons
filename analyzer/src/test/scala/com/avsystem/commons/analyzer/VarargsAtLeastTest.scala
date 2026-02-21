package com.avsystem.commons
package analyzer

import org.scalatest.funsuite.AnyFunSuite

final class VarargsAtLeastTest extends AnyFunSuite with AnalyzerTest {

  test("too few varargs parameters should be rejected") {
    assertErrors(
      1,
      scala"""
             |import com.avsystem.commons.analyzer.TestUtils
             |TestUtils.need3Params(1, 2)
             |""".stripMargin,
    )
  }

  test("enough varargs parameters should not be rejected") {
    assertNoErrors(
      scala"""
             |import com.avsystem.commons.analyzer.TestUtils
             |TestUtils.need3Params(1, 2, 3)
             |TestUtils.need3Params(1, 2, 3, 4)
             |""".stripMargin,
    )
  }

  test("collection passed as varargs parameter should not be rejected") {
    assertNoErrors(
      scala"""
             |import com.avsystem.commons.analyzer.TestUtils
             |TestUtils.need3Params(List(1, 2)*)
             |""".stripMargin,
    )
  }
}
