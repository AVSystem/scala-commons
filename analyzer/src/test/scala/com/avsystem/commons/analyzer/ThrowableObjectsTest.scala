package com.avsystem.commons
package analyzer

import org.scalatest.funsuite.AnyFunSuite

final class ThrowableObjectsTest extends AnyFunSuite with AnalyzerTest {
  test("throwable objects with stack trace should be rejected") {
    assertErrors(
      1,
      scala"""
             |object throwableObject extends Throwable
             |object noStackTraceThrowableObject extends Throwable with scala.util.control.NoStackTrace
             |""".stripMargin,
    )
  }
}
