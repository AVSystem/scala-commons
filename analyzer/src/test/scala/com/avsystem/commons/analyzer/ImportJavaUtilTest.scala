package com.avsystem.commons
package analyzer


import org.scalatest.funsuite.AnyFunSuite

final class ImportJavaUtilTest extends AnyFunSuite with AnalyzerTest {
  test("import java.util should be rejected") {
    assertErrors(1, scala"import java.util")
  }
}
