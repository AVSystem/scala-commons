package com.avsystem.commons
package analyzer

import org.scalatest.funsuite.AnyFunSuite

final class FinalCaseClassesTest extends AnyFunSuite with AnalyzerTest {
  test("final case class should pass") {
    assertNoErrors(scala"""
                          |final case class GoodCaseClass(x: Int, y: String) {
                          |  def double: Int = x * 2
                          |}
                          |""".stripMargin)
  }

  test("case class not marked as final should fail") {
    assertErrors(
      1,
      scala"""
             |case class BadCaseClass1(x: Int, y: String) {
             |  def double: Int = x * 2
             |}
             |""".stripMargin,
    )
  }

  test("generic case class not marked as final should fail") {
    assertErrors(
      1,
      scala"""
             |case class BadCaseClass2[T](x: T, y: String) {
             |  def double: String = y * 2
             |}
             |""".stripMargin,
    )
  }

  test("regular class should not be affected") {
    assertNoErrors(scala"""
                          |class RegularClass(val x: Int, val y: String) {
                          |  def double: Int = x * 2
                          |}
                          |""".stripMargin)
  }

  test("regular class with case-like constructor should not be affected") {
    assertNoErrors(scala"""
                          |class RegularClass2(x: Int, y: String) {
                          |  def double: Int = x * 2
                          |}
                          |""".stripMargin)
  }

  test("inner case class in trait should be flagged") {
    assertErrors(
      1,
      scala"""
             |trait Outer {
             |  case class Inner(x: Int, y: String) {
             |    def double: Int = x * 2
             |  }
             |}
             |""".stripMargin,
    )
  }

  test("inner case class in class should be flagged") {
    assertErrors(
      1,
      scala"""
             |class Outer2 {
             |  case class Inner(x: Int, y: String) {
             |    def double: Int = x * 2
             |  }
             |}
             |""".stripMargin,
    )
  }

  test("sealed case class should not be affected") {
    assertNoErrors(
      scala"""
             |sealed case class SealedCaseClass(x: Int) {
             |  def double: Int = x * 2
             |}
             |object SealedCaseClass {
             |  val jeden = SealedCaseClass(1)
             |  val dwa = SealedCaseClass(2)
             |}
             |""".stripMargin,
    )
  }
}
