package com.avsystem.commons
package analyzer

import org.scalatest.funsuite.AnyFunSuite

final class CanaryTest extends AnyFunSuite with AnalyzerTest {

  test("plugin initializes and compiles valid code without errors") {
    // Proves: ContextBase plugin injection works, Compiler runs,
    // and valid code produces no errors with the plugin loaded.
    val result = compile("object ValidCode { def x: Int = 42 }")
    assert(!result.hasErrors, s"Valid code should compile without errors: ${result.errors.mkString("; ")}")
  }

  test("valid code without plugin produces zero diagnostics") {
    // Proves: The compileWithoutPlugin path works and establishes
    // the baseline that the standard compiler produces no diagnostics
    // for valid code.
    val result = compileWithoutPlugin("object ValidCode { val x: Int = 42 }")
    assert(!result.hasErrors, s"Without plugin, valid code should have no errors: ${result.errors.mkString("; ")}")
    assert(!result.hasWarnings, s"Without plugin, valid code should have no warnings: ${result.warnings.mkString("; ")}")
  }

  test("reporter captures compiler warnings") {
    // Proves: The Reporter correctly captures warning-level diagnostics
    // with their message text. Uses a non-exhaustive pattern match which
    // produces a standard compiler warning (not plugin-specific).
    // This validates TEST-02: diagnostic capture at warning level.
    val source =
      """object WarnTest {
        |  sealed trait Color
        |  case object Red extends Color
        |  case object Blue extends Color
        |  def test(c: Color): Int = c match {
        |    case Red => 1
        |  }
        |}""".stripMargin
    val result = compile(source)
    assert(
      result.warningCount > 0,
      s"Should capture at least one match-not-exhaustive warning, got ${result.warningCount}",
    )
    assert(
      result.warnings.exists(msg =>
        msg.toLowerCase.contains("match") || msg.toLowerCase.contains("not exhaustive") ||
          msg.toLowerCase.contains("blue"),
      ),
      s"Warning messages should mention non-exhaustive match, got: ${result.warnings.mkString("; ")}",
    )
  }

  test("reporter captures compiler errors") {
    // Proves: The Reporter correctly captures error-level diagnostics.
    // Uses a type mismatch which produces a standard compiler error.
    // This validates TEST-02: diagnostic capture at error level.
    val source =
      """object ErrorTest {
        |  val x: Int = "not an int"
        |}""".stripMargin
    val result = compile(source)
    assert(result.hasErrors, "Type mismatch should produce at least one error")
    assert(result.errorCount > 0, s"Expected errors, got 0. Warnings: ${result.warnings.mkString("; ")}")
  }

  test("compilation without plugin does not produce plugin diagnostics") {
    // Proves: The test is not vacuously green. When compiling without
    // the plugin, no [AVS]-prefixed diagnostics appear. This is the
    // foundation for the Phase 3+ canary that will assert diagnostics
    // ARE produced with the plugin but NOT without it.
    val source = "object SimpleCode { def x: Int = 42 }"
    val withPlugin = compile(source)
    val withoutPlugin = compileWithoutPlugin(source)
    // Both should have zero errors on valid code
    assert(!withPlugin.hasErrors, s"With plugin, valid code should have no errors: ${withPlugin.errors.mkString("; ")}")
    assert(
      !withoutPlugin.hasErrors,
      s"Without plugin, valid code should have no errors: ${withoutPlugin.errors.mkString("; ")}",
    )
    // Without plugin should have no [AVS] diagnostics
    val allWithoutPluginMessages = withoutPlugin.errors ++ withoutPlugin.warnings
    assert(
      !allWithoutPluginMessages.exists(_.contains("[AVS]")),
      "Without plugin, no [AVS]-prefixed diagnostics should appear",
    )
  }

  test("fresh context per compilation avoids stale state") {
    // Proves: Each compile() call gets a fresh ContextBase so
    // diagnostics from one compilation do not leak into the next.
    val errorSource =
      """object Bad {
        |  val x: Int = "not an int"
        |}""".stripMargin
    val validSource = "object Good { def x: Int = 42 }"

    val result1 = compile(errorSource)
    assert(result1.hasErrors, "First compilation should have errors")

    val result2 = compile(validSource)
    assert(
      !result2.hasErrors,
      s"Second compilation should have zero errors (fresh context), but got: ${result2.errors.mkString("; ")}",
    )
    assert(
      result2.errorCount == 0,
      s"Error count should be 0 for valid code after prior error compilation, got ${result2.errorCount}",
    )
  }
}
