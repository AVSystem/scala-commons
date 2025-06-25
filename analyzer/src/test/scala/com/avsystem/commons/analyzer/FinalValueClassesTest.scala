package com.avsystem.commons
package analyzer

import org.scalatest.funsuite.AnyFunSuite

final class FinalValueClassesTest extends AnyFunSuite with AnalyzerTest {
  test("final value class should pass") {
    assertNoErrors(
      scala"""
             |final class GoodValueClass(val x: Int) extends AnyVal {
             |  def double: Int = x * 2
             |}
             |""".stripMargin)
  }

  test("value class not marked as final should fail") {
    assertErrors(1,
      scala"""
             |class BadValueClass1(val x: Int) extends AnyVal {
             |  def double: Int = x * 2
             |}
             |""".stripMargin)
  }

  test("generic value class not marked as final should fail") {
    assertErrors(1,
      scala"""
             |class BadValueClass2[T <: Int](val x: T) extends AnyVal {
             |  def double: Int = x * 2
             |}
             |""".stripMargin)
  }

  test("regular class with multiple parameters should not be affected") {
    assertNoErrors(
      scala"""
             |class RegularClass(val x: Int, val y: Int) {
             |  def double: Int = x * 2
             |}
             |""".stripMargin)
  }

  test("regular class not extending AnyVal should not be affected") {
    assertNoErrors(
      scala"""
             |class RegularClass2(val x: Int) {
             |  def double: Int = x * 2
             |}
             |""".stripMargin)
  }
}
