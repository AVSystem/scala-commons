package com.avsystem.commons
package analyzer

import org.scalatest.FunSuite

class ValueEnumExhaustiveMatchTest extends FunSuite with AnalyzerTest {
  def source(caseDefs: String): String =
    s"""
       |import com.avsystem.commons.misc._
       |
       |final class Enumz(implicit enumCtx: EnumCtx) extends AbstractValueEnum
       |object Enumz extends AbstractValueEnumCompanion[Enumz] {
       |  final val One, Two, Three: Value = new Enumz
       |}
       |
       |object Main {
       |  val enum: Enumz = Enumz.One
       |  import Enumz._
       |  enum match {
       |    $caseDefs
       |  }
       |}
      """.stripMargin

  test("should report two unmatched enum values") {
    assertErrors(source(
      """
        |case Enumz.One =>
        |case null =>
      """.stripMargin
    ))
  }

  test("should report one unmatched enum value") {
    assertErrors(source(
      """
        |case Enumz.One =>
        |case Enumz.Two =>
      """.stripMargin
    ))
  }

  test("should report one unmatched by alternative enum value") {
    assertErrors(source(
      """
        |case One | Two =>
      """.stripMargin
    ))
  }

  test("should not report unmatched values on wildcard") {
    assertNoErrors(source(
      """
        |case _ =>
      """.stripMargin
    ))
  }

  test("should not report unmatched values with guard") {
    assertNoErrors(source(
      """
        |case x if x.ordinal > 1 =>
      """.stripMargin
    ))
  }

  test("should not report no unmatched values in alternative") {
    assertNoErrors(source(
      """
        |case One | Two | Three =>
      """.stripMargin
    ))
  }

  test("should not report no unmatched values") {
    assertNoErrors(source(
      """
        |case Enumz.One =>
        |case Enumz.Two =>
        |case Three =>
      """.stripMargin
    ))
  }
}
