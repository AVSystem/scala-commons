package com.avsystem.commons
package analyzer

import org.scalatest.funsuite.AnyFunSuite

final class BasePackageTest extends AnyFunSuite with AnalyzerTest {

  override protected def pluginOptions: List[String] =
    List("AVSystemAnalyzer:+_", "AVSystemAnalyzer:+basePackage:com.avsystem")

  test("file with required base package should pass") {
    assertNoErrors(
      "package com.avsystem\n" + "object Good { def x = 1 }",
    )
  }

  test("file with wrong base package should be rejected") {
    assertErrors(
      1,
      "package com.other\n" + "object Bad { def x = 1 }",
    )
  }

  test("file with nested required base package should pass") {
    assertNoErrors(
      "package com\n" + "package avsystem\n" + "object Nested { def x = 1 }",
    )
  }
}
