package com.avsystem.commons
package analyzer

import org.scalatest.funsuite.AnyFunSuite

final class ImplicitValueClassesTest extends AnyFunSuite with AnalyzerTest {

  override protected def pluginOptions: List[String] =
    List("AVSystemAnalyzer:-_", "AVSystemAnalyzer:+implicitValueClasses")

  test("implicit class not extending AnyVal should warn") {
    assertErrors(
      1,
      scala"""
      implicit class RichInt(val n: Int) {
        def double: Int = n * 2
      }
    """,
    )
  }

  test("implicit class extending AnyVal should not warn") {
    assertErrors(
      0,
      scala"""
      implicit class RichInt(val n: Int) extends AnyVal {
        def double: Int = n * 2
      }
    """,
    )
  }

  test("non-implicit class should not warn") {
    assertErrors(
      0,
      scala"""
      class NotImplicit(val n: Int) {
        def double: Int = n * 2
      }
    """,
    )
  }

  test("implicit class with String param not extending AnyVal should warn") {
    assertErrors(
      1,
      scala"""
      implicit class RichString(val s: String) {
        def shout: String = s.toUpperCase
      }
    """,
    )
  }
}
