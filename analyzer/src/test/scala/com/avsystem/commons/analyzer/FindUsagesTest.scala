package com.avsystem.commons
package analyzer

import org.scalatest.funsuite.AnyFunSuite

final class FindUsagesTest extends AnyFunSuite with AnalyzerTest {
  override protected def pluginOptions: List[String] =
    List("AVSystemAnalyzer:+_", "AVSystemAnalyzer:+findUsages:java.lang.String")

  test("term-level usages of tracked symbol should be found") {
    assertErrors(
      1,
      scala"""val x = String.valueOf(123)""",
    )
  }

  test("multiple usages of tracked symbol should all be found") {
    assertErrors(
      2,
      scala"""
             |val x = String.valueOf(123)
             |val y = String.valueOf(456)""".stripMargin,
    )
  }

  test("code not using tracked symbol should pass") {
    assertNoErrors(
      scala"""def x: Int = 42""",
    )
  }
}
