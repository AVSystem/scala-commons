package com.avsystem.commons
package analyzer

import org.scalatest.funsuite.AnyFunSuite

final class Any2StringAddTest extends AnyFunSuite with AnalyzerTest {
  test("any2stringadd should be rejected") {
    assertErrors(
      1,
      scala"""
             |val any: Any = ???
             |any + "fag"
             |""".stripMargin,
    )
  }

  test("toString should not be rejected") {
    assertNoErrors(
      scala"""
             |val any: Any = ???
             |any.toString + "fag"
             |""".stripMargin
    )
  }

  test("string interpolation should not be rejected") {
    assertNoErrors(scala"""
             |val any: Any = ???
             |s"$${any}fag"
             |""".stripMargin)
  }
}
