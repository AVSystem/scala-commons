package com.avsystem.commons
package analyzer


import org.scalatest.funsuite.AnyFunSuite

final class CheckMacroPrivateTest extends AnyFunSuite with AnalyzerTest {
  test("macro private method invoked directly should be rejected") {
    assertErrors(1,
      scala"""
             |import com.avsystem.commons.analyzer.TestUtils
             |
             |object test {
             |  TestUtils.macroPrivateMethod
             |}
             |""".stripMargin
    )
  }

  test("macro private extractor used directly should be rejected") {
    assertErrors(1,
      scala"""
             |import com.avsystem.commons.analyzer.TestUtils
             |
             |object test {
             |  123 match {
             |    case TestUtils.Extractor(_) =>
             |  }
             |}
             |""".stripMargin
    )
  }

  test("macro private method invoked by macro-generated code should not be rejected") {
    assertNoErrors(
      scala"""
             |import com.avsystem.commons.analyzer.TestUtils
             |
             |object test {
             |  TestUtils.invokeMacroPrivateMethod
             |}
             |""".stripMargin
    )
  }

  test("definitions of macro private symbols themselves should not be rejected") {
    assertNoErrors(
      scala"""
             |import com.avsystem.commons.annotation.macroPrivate
             |
             |object test {
             |  @macroPrivate def macroPrivateMethod = { println("whatever"); 5 }
             |  @macroPrivate object macroPrivateObject {
             |    final val X = 42
             |  }
             |}
             |""".stripMargin
    )
  }
}
