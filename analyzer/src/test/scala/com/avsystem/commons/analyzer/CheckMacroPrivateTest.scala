package com.avsystem.commons
package analyzer

import org.scalatest.funsuite.AnyFunSuite

final class CheckMacroPrivateTest extends AnyFunSuite with AnalyzerTest {

  // Only enable the macroPrivate rule to avoid interference from other rules
  override protected def pluginOptions: List[String] = List("AVSystemAnalyzer:+macroPrivate")

  test("usage of @macroPrivate symbol outside inline method should be rejected") {
    assertErrors(
      1,
      scala"""
             |import com.avsystem.commons.annotation.macroPrivate
             |
             |@macroPrivate def secret: Int = 42
             |val x = secret
             |""".stripMargin,
    )
  }

  test("usage of @macroPrivate symbol inside inline method should be allowed") {
    assertNoErrors(
      scala"""
             |import com.avsystem.commons.annotation.macroPrivate
             |
             |@macroPrivate def secret: Int = 42
             |inline def macroLike: Int = secret
             |""".stripMargin,
    )
  }

  test("definition of @macroPrivate symbol itself should not be rejected") {
    assertNoErrors(
      scala"""
             |import com.avsystem.commons.annotation.macroPrivate
             |
             |@macroPrivate def secret: Int = 42
             |""".stripMargin,
    )
  }

  test("@macroPrivate val used outside inline method should be rejected") {
    assertErrors(
      1,
      scala"""
             |import com.avsystem.commons.annotation.macroPrivate
             |
             |@macroPrivate val secret: Int = 42
             |val x = secret
             |""".stripMargin,
    )
  }

  test("usage inside nested inline method should be allowed") {
    assertNoErrors(
      scala"""
             |import com.avsystem.commons.annotation.macroPrivate
             |
             |@macroPrivate def secret: Int = 42
             |object Wrapper {
             |  inline def macroLike: Int = secret
             |}
             |""".stripMargin,
    )
  }
}
