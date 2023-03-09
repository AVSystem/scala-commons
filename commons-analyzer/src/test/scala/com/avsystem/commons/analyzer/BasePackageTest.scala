package com.avsystem.commons
package analyzer

import org.scalatest.funsuite.AnyFunSuite

class BasePackageTest extends AnyFunSuite with AnalyzerTest {
  settings.pluginOptions.value ++= List("AVSystemAnalyzer:+basePackage:com.avsystem.commons")

  test("base package only") {
    assertNoErrors(
      """
        |package com.avsystem.commons
        |
        |object bar
        |""".stripMargin)
  }

  test("chained base package") {
    assertNoErrors(
      """
        |package com.avsystem
        |package commons
        |
        |object bar
        |""".stripMargin)
  }

  test("base package with chained subpackage") {
    assertNoErrors(
      """
        |package com.avsystem.commons
        |package core
        |
        |object bar
        |""".stripMargin)
  }

  test("base package object") {
    assertNoErrors(
      """
        |package com.avsystem
        |
        |package object commons
        |""".stripMargin)
  }

  test("no base package") {
    assertErrors(1,
      """
        |object bar
        |""".stripMargin)
  }

  test("wrong base package") {
    assertErrors(1,
      """
        |package com.avsystem.kommons
        |
        |object bar
        |""".stripMargin)
  }

  test("unchained subpackage") {
    assertErrors(1,
      """
        |package com.avsystem.commons.core
        |
        |object bar
        |""".stripMargin)
  }
}
