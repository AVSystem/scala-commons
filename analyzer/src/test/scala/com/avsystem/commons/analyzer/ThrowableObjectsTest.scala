package com.avsystem.commons
package analyzer

import org.scalatest.funsuite.AnyFunSuite

final class ThrowableObjectsTest extends AnyFunSuite with AnalyzerTest {
  test("throwable object without fillInStackTrace override should be rejected") {
    assertErrors(
      1,
      scala"""
             |object throwableObject extends Throwable
             |""".stripMargin,
    )
  }

  test("throwable object with NoStackTrace should be allowed") {
    assertNoErrors(scala"""
                          |object noStackTraceThrowableObject extends Throwable with scala.util.control.NoStackTrace
                          |""".stripMargin)
  }

  test("throwable objects - mixed (one with override, one without)") {
    assertErrors(
      1,
      scala"""
             |object throwableObject extends Throwable
             |object noStackTraceThrowableObject extends Throwable with scala.util.control.NoStackTrace
             |""".stripMargin,
    )
  }

  test("regular object not extending Throwable should be allowed") {
    assertNoErrors(scala"""
                          |object regularObject {
                          |  def x: Int = 42
                          |}
                          |""".stripMargin)
  }

  test("throwable class (not object) should not be flagged") {
    assertNoErrors(scala"""
                          |class MyException extends Throwable
                          |""".stripMargin)
  }
}
