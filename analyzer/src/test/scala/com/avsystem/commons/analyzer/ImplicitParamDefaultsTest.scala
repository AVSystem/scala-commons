package com.avsystem.commons
package analyzer

import org.scalatest.funsuite.AnyFunSuite

final class ImplicitParamDefaultsTest extends AnyFunSuite with AnalyzerTest {

  private val DummyDefinition =
    """
      |class Scheduler
      |object Scheduler {
      |  val global = new Scheduler
      |}
      |""".stripMargin

  test("implicit parameter without default value should pass") {
    assertNoErrors(scala"$DummyDefinition def goodMethod1(implicit s: Scheduler): Unit = ???")
  }

  test("regular parameter with default value should pass") {
    assertNoErrors(scala"$DummyDefinition def goodMethod2(s: Scheduler = Scheduler.global): Unit = ???")
  }

  test("implicit parameter with default value should fail") {
    assertErrors(1, scala"$DummyDefinition def badMethod1(implicit s: Scheduler = Scheduler.global): Unit = ???")
  }

  test("implicit parameter with default value in second parameter list should fail") {
    assertErrors(
      1,
      scala"$DummyDefinition def badMethod(sth: Int)(implicit s: Scheduler = Scheduler.global): Unit = ???",
    )
  }

  test("generic method with implicit parameter with default value should fail") {
    assertErrors(
      1,
      scala"$DummyDefinition def badMethod2[T](x: T)(implicit s: Scheduler = Scheduler.global): T = ???",
    )
  }

  test("implicit class parameter without default value should pass") {
    assertNoErrors(scala"$DummyDefinition class GoodClass1(implicit s: Scheduler)")
  }

  test("regular class parameter with default value should pass") {
    assertNoErrors(scala"$DummyDefinition class GoodClass2(s: Scheduler = Scheduler.global)")
  }

  test("implicit class parameter with default value should fail") {
    assertErrors(1, scala"$DummyDefinition class BadClass1(implicit s: Scheduler = Scheduler.global)")
  }

  test("implicit class parameter with default value in second parameter list should fail") {
    assertErrors(
      1,
      scala"$DummyDefinition class BadClass2(sth: Int)(implicit s: Scheduler = Scheduler.global)",
    )
  }

  test("generic class with implicit parameter with default value should fail") {
    assertErrors(
      1,
      scala"$DummyDefinition class BadClass3[T](x: T)(implicit s: Scheduler = Scheduler.global)",
    )
  }

  test("given parameter (Scala 3 syntax) without default should pass") {
    assertNoErrors(scala"""
      def bar(using x: Int): Int = x
    """)
  }

  test("given parameter with default value should fail") {
    assertErrors(
      1,
      scala"""
      def bar(using x: Int = 42): Int = x
    """,
    )
  }
}
