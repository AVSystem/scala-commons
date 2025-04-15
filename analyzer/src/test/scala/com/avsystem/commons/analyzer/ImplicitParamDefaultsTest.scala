package com.avsystem.commons
package analyzer

import org.scalatest.funsuite.AnyFunSuite

class ImplicitParamDefaultsTest extends AnyFunSuite with AnalyzerTest {
  test("implicit parameters should not have default values") {
    assertErrors(6,
      """
        |object whatever {
        |  // This should pass - implicit parameter without default value
        |  def goodMethod1(implicit s: Scheduler): Unit = ???
        |
        |  // This should pass - regular parameter with default value
        |  def goodMethod2(s: Scheduler = Scheduler.global): Unit = ???
        |
        |  // This should fail - implicit parameter with default value
        |  def badMethod1(implicit s: Scheduler = Scheduler.global): Unit = ???
        |
        |  // This should fail - implicit parameter with default value
        |  def badMethod(sth: Int)(implicit s: Scheduler = Scheduler.global): Unit = ???
        |
        |  // This should fail - another implicit parameter with default value
        |  def badMethod2[T](x: T)(implicit s: Scheduler = Scheduler.global): T = ???
        |
        |  // This should pass - implicit parameter without default value
        |  class GoodClass1(implicit s: Scheduler)
        |
        |  // This should pass - regular parameter with default value
        |  class GoodClass2(s: Scheduler = Scheduler.global)
        |
        |  // This should fail - implicit parameter with default value
        |  class BadClass1(implicit s: Scheduler = Scheduler.global)
        |
        |  // This should fail - implicit parameter with default value
        |  class BadClass2(sth: Int)(implicit s: Scheduler = Scheduler.global)
        |
        |  // This should fail - another implicit parameter with default value
        |  class BadClass3[T](x: T)(implicit s: Scheduler = Scheduler.global)
        |
        |  // Dummy classes for the test
        |  class Scheduler
        |  object Scheduler {
        |    val global = new Scheduler
        |  }
        |}
      """.stripMargin)
  }
}