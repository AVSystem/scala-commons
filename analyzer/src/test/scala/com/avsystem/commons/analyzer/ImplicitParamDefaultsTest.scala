package com.avsystem.commons
package analyzer

import org.scalatest.funsuite.AnyFunSuite

final class ImplicitParamDefaultsTest extends AnyFunSuite with AnalyzerTest {
  private val DummyDefinition = {
    //language=Scala
    """
      |class Scheduler
      |object Scheduler {
      | val global = new Scheduler
      |}
      |""".stripMargin
  }

  test("implicit parameter without default value should pass") {
    assertNoErrors(DummyDefinition +
      //language=Scala
      """
        |object whatever {
        |  def goodMethod1(implicit s: Scheduler): Unit = ???
        |}
      """.stripMargin)
  }

  test("regular parameter with default value should pass") {
    assertNoErrors(DummyDefinition +
      //language=Scala
      """
        |object whatever {
        |  // This should pass - regular parameter with default value
        |  def goodMethod2(s: Scheduler = Scheduler.global): Unit = ???
        |}
      """.stripMargin)
  }

  test("implicit parameter with default value should fail") {
    assertErrors(1, DummyDefinition +
      //language=Scala
      """
        |object whatever {
        |  def badMethod1(implicit s: Scheduler = Scheduler.global): Unit = ???
        |}
      """.stripMargin)
  }

  test("implicit parameter with default value in second parameter list should fail") {
    assertErrors(1, DummyDefinition +
      //language=Scala
      """
        |object whatever {
        |  def badMethod(sth: Int)(implicit s: Scheduler = Scheduler.global): Unit = ???
        |}
      """.stripMargin)
  }

  test("generic method with implicit parameter with default value should fail") {
    assertErrors(1, DummyDefinition +
      //language=Scala
      """
        |object whatever {
        |  def badMethod2[T](x: T)(implicit s: Scheduler = Scheduler.global): T = ???
        |}
      """.stripMargin)
  }

  test("implicit class parameter without default value should pass") {
    assertNoErrors(DummyDefinition +
      //language=Scala
      """
        |class GoodClass1(implicit s: Scheduler)
      """.stripMargin)
  }

  test("regular class parameter with default value should pass") {
    assertNoErrors(DummyDefinition +
      //language=Scala
      """
        |class GoodClass2(s: Scheduler = Scheduler.global)
      """.stripMargin)
  }

  test("implicit class parameter with default value should fail") {
    assertErrors(1, DummyDefinition +
      //language=Scala
      """
        |class BadClass1(implicit s: Scheduler = Scheduler.global)
      """.stripMargin)
  }

  test("implicit class parameter with default value in second parameter list should fail") {
    assertErrors(1, DummyDefinition +
      //language=Scala
      """
        |class BadClass2(sth: Int)(implicit s: Scheduler = Scheduler.global)
      """.stripMargin)
  }

  test("generic class with implicit parameter with default value should fail") {
    assertErrors(1, DummyDefinition +
      //language=Scala
      """
        |class BadClass3[T](x: T)(implicit s: Scheduler = Scheduler.global)
      """.stripMargin)
  }
}
