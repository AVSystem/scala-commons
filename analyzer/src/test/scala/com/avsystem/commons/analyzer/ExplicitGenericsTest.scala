package com.avsystem.commons
package analyzer

import org.scalatest.funsuite.AnyFunSuite

final class ExplicitGenericsTest extends AnyFunSuite with AnalyzerTest {
  test("inferred generic should be rejected") {
    assertErrors(
      1,
      scala"""
             |import com.avsystem.commons.analyzer.TestUtils
             |
             |val x = TestUtils.genericMethod(123)
             |""".stripMargin,
    )
  }

  test("inferred generic in macro should be rejected") {
    assertErrors(
      1,
      scala"""
             |import com.avsystem.commons.analyzer.TestUtils
             |
             |val x = TestUtils.genericMacro(123)
             |""".stripMargin,
    )
  }

  test("explicit generic should not be rejected") {
    assertNoErrors(scala"""
             |import com.avsystem.commons.analyzer.TestUtils
             |
             |val x = TestUtils.genericMethod[Int](123)
             |""".stripMargin)
  }

  test("explicit generic in macro should not be rejected") {
    assertNoErrors(scala"""
             |import com.avsystem.commons.analyzer.TestUtils
             |
             |val x = TestUtils.genericMacro[Int](123)
             |""".stripMargin)
  }

  test("inferred in constructor should be rejected") {
    assertErrors(2,
      scala"""
             |import com.avsystem.commons.analyzer.TestUtils
             |
             |val x = new TestUtils.GenericClass()
             |val y = new TestUtils.GenericCaseClass(123)
             |""".stripMargin)
  }


  test("inferred in apply when constructor marked should be rejected") {
    assertErrors(1,
      scala"""
             |import com.avsystem.commons.analyzer.TestUtils
             |
             |val x = TestUtils.GenericCaseClass(123)
             |""".stripMargin)
  }

  test("explicit in constructor should not be rejected") {
    assertNoErrors(
      scala"""
             |import com.avsystem.commons.analyzer.TestUtils
             |
             |val x = new TestUtils.GenericClass[Int]()
             |""".stripMargin)
  }

  test("not marked should not be rejected") {
    assertNoErrors(
      scala"""
             |def method[T](e: T) = e
             |class NotMarkedGenericClass[T]
             |final case class NotMarkedGenericCaseClass[T](arg: T)
             |
             |val w = method(123)
             |val x = new NotMarkedGenericClass()
             |val y = NotMarkedGenericCaseClass(123)
             |val z = new NotMarkedGenericClass()
             |""".stripMargin)
  }
}
