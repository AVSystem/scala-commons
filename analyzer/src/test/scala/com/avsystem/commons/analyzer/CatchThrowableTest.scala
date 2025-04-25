package com.avsystem.commons
package analyzer

import org.scalatest.funsuite.AnyFunSuite

class CatchThrowableTest extends AnyFunSuite with AnalyzerTest {
  test("catching Throwable should be rejected") {
    assertErrors(1,
      //language=Scala
      """
        |object Test {
        |  def test(): Unit = {
        |    try {
        |      println("test")
        |    } catch {
        |      case t: Throwable => println(t)
        |    }
        |  }
        |}
      """.stripMargin)
  }

  test("catching specific exceptions should be allowed") {
    assertNoErrors(
      //language=Scala
      """
        |object Test {
        |  def test(): Unit = {
        |    try {
        |      println("test")
        |    } catch {
        |      case e: Exception => println(e)
        |      case e: RuntimeException => println(e)
        |      case e: IllegalArgumentException => println(e)
        |    }
        |  }
        |}
      """.stripMargin)
  }

  test("catching Throwable with other exceptions should be rejected") {
    assertErrors(1,
      //language=Scala
      """
        |object Test {
        |  def test(): Unit = {
        |    try {
        |      println("test")
        |    } catch {
        |      case e: IllegalArgumentException => println(e)
        |      case t: Throwable => println(t)
        |    }
        |  }
        |}
      """.stripMargin)
  }

  test("catching Throwable in nested catch block should be rejected") {
    assertErrors(1,
      //language=Scala
      """
        |object Test {
        |  def test(): Unit = {
        |    try println("test")
        |    catch {
        |      case e: Exception => try println("test")
        |      catch {
        |        case e: Throwable => println(e)
        |      }
        |    }
        |  }
        |}
      """.stripMargin)
  }

  test("catching Throwable using NonFatal should be allowed") {
    assertNoErrors(
      //language=Scala
      """
        |object Test extends com.avsystem.commons.CommonAliases {
        |  def test(): Unit = {
        |    try {
        |      println("test")
        |    } catch {
        |      case NonFatal(t) => println(t)
        |      case scala.util.control.NonFatal(t) => println(t)
        |    }
        |  }
        |}
      """.stripMargin)
  }

  test("catching non-Throwable with pattern match should be allowed") {
    assertNoErrors(
      //language=Scala
      """
        |object Test {
        |  def test(): Unit = {
        |    try {
        |      println("test")
        |    } catch {
        |      case _: IndexOutOfBoundsException | _: NullPointerException => println("OK!")
        |    }
        |    try {
        |      println("test")
        |    } catch {
        |      case e@(_: IndexOutOfBoundsException | _: NullPointerException) => println("OK!")
        |    }
        |  }
        |}
      """.stripMargin)
  }

  test("catching Throwable with pattern match should be rejected") {
    assertErrors(1,
      //language=Scala
      """
        |object Test {
        |  def test(): Unit = {
        |    try {
        |      println("test")
        |    } catch {
        |      case _: IndexOutOfBoundsException | _: Throwable => println("Not OK!")
        |    }
        |  }
        |}
      """.stripMargin)
  }
}
