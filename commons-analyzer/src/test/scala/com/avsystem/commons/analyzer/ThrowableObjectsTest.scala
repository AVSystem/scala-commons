package com.avsystem.commons
package analyzer

import org.scalatest.FunSuite

class ThrowableObjectsTest extends FunSuite with AnalyzerTest {
  test("throwable objects with stack trace should be rejected") {
    assertErrors(1,
      """
        |object throwableObject extends Throwable
        |object noStackTraceThrowableObject extends Throwable with scala.util.control.NoStackTrace
      """.stripMargin)
  }
}
