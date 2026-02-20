package com.avsystem.commons
package analyzer

import org.scalatest.funsuite.AnyFunSuite

final class FinalValueClassesTest extends AnyFunSuite with AnalyzerTest {
  // NOTE: In Scala 3, the compiler automatically adds the Final flag to all value classes
  // (classes extending AnyVal) before the plugin phase runs. This means non-final value
  // classes in source code appear as `final` in the typed tree, making this rule a no-op.
  // We test that the rule does not produce false positives and is forward-compatible.

  test("final value class should pass") {
    assertNoErrors(scala"""
                          |final class GoodValueClass(val x: Int) extends AnyVal {
                          |  def double: Int = x * 2
                          |}
                          |""".stripMargin)
  }

  test("value class not marked as final should not trigger in Scala 3 (compiler auto-finalizes)") {
    // In Scala 3, value classes are automatically final even without the keyword.
    // The compiler adds Final flag before our phase runs, so our rule sees it as final.
    assertNoErrors(scala"""
                          |class ValueClass1(val x: Int) extends AnyVal {
                          |  def double: Int = x * 2
                          |}
                          |""".stripMargin)
  }

  test("generic value class should not trigger in Scala 3 (compiler auto-finalizes)") {
    // Same as above: Scala 3 automatically finalizes value classes.
    assertNoErrors(scala"""
                          |class ValueClass2[T <: Int](val x: T) extends AnyVal {
                          |  def double: Int = x * 2
                          |}
                          |""".stripMargin)
  }

  test("regular class with multiple parameters should not be affected") {
    assertNoErrors(scala"""
                          |class RegularClass(val x: Int, val y: Int) {
                          |  def double: Int = x * 2
                          |}
                          |""".stripMargin)
  }

  test("regular class not extending AnyVal should not be affected") {
    assertNoErrors(scala"""
                          |class RegularClass2(val x: Int) {
                          |  def double: Int = x * 2
                          |}
                          |""".stripMargin)
  }
}
