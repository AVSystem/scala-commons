package com.avsystem.commons
package analyzer


import org.scalatest.funsuite.AnyFunSuite

final class FindUsagesTest extends AnyFunSuite with AnalyzerTest {
  settings.pluginOptions.value ++= List("AVSystemAnalyzer:+findUsages:java.lang.String")

  test("java.lang.String usages should be found") {
    assertErrors(2, scala"val x: String = String.valueOf(123)")
  }
}
