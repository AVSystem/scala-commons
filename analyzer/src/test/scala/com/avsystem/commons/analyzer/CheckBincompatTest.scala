package com.avsystem.commons
package analyzer

import org.scalatest.funsuite.AnyFunSuite

final class CheckBincompatTest extends AnyFunSuite with AnalyzerTest {

  test("definitions of @bincompat annotated symbols should not be rejected") {
    assertNoErrors(
      scala"""
             |import com.avsystem.commons.annotation.bincompat
             |
             |@bincompat class klass
             |
             |@bincompat object objekt {
             |  @bincompat def method: Int = 42
             |}
             |""".stripMargin,
    )
  }

  test("usage of @bincompat annotated symbols should be rejected") {
    assertErrors(
      3,
      scala"""
             |import com.avsystem.commons.annotation.bincompat
             |
             |@bincompat class klass
             |
             |@bincompat object objekt
             |
             |object outer {
             |  @bincompat def method: Int = 42
             |}
             |
             |object test {
             |  println(objekt)
             |  println(new klass)
             |  println(outer.method)
             |}
             |""".stripMargin,
    )
  }
}
