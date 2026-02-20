package com.avsystem.commons
package analyzer

import org.scalatest.funsuite.AnyFunSuite

final class CatchThrowableTest extends AnyFunSuite with AnalyzerTest {
  test("catching Throwable should be rejected") {
    assertErrors(
      1,
      scala"""
             |try {
             |  println("test")
             |} catch {
             |  case t: Throwable => println(t)
             |}
             |""".stripMargin,
    )
  }

  test("catching specific exceptions should be allowed") {
    assertNoErrors(scala"""
                          |try {
                          |  println("test")
                          |} catch {
                          |  case e: Exception => println(e)
                          |  case e: RuntimeException => println(e)
                          |  case e: IllegalArgumentException => println(e)
                          |}
                          |""".stripMargin)
  }

  test("catching Throwable with other exceptions should be rejected") {
    assertErrors(
      1,
      scala"""
             |try {
             |  println("test")
             |} catch {
             |  case e: IllegalArgumentException => println(e)
             |  case t: Throwable => println(t)
             |}
             |""".stripMargin,
    )
  }

  test("catching Throwable in nested catch block should be rejected") {
    assertErrors(
      1,
      scala"""
             |try println("test")
             |catch {
             |  case e: Exception => try println("test")
             |  catch {
             |    case e: Throwable => println(e)
             |  }
             |}
             |""".stripMargin,
    )
  }

  test("catching Throwable using custom extractor should be allowed") {
    assertNoErrors(scala"""
                          |import scala.util.control.NonFatal
                          |
                          |object custom {
                          |  def unapply(t: Throwable): Option[IllegalArgumentException] = t match {
                          |    case e: IllegalArgumentException => Some(e)
                          |    case _ => None
                          |  }
                          |}
                          |
                          |try {
                          |  println("test")
                          |} catch {
                          |  case custom(t) => println(t)
                          |  case NonFatal(t) => println(t)
                          |  case scala.util.control.NonFatal(t) => println(t)
                          |}
                          |""".stripMargin)
  }

  test("catching non-Throwable with pattern match should be allowed") {
    assertNoErrors(
      scala"""
             |try {
             |  println("test")
             |} catch {
             |  case _: IndexOutOfBoundsException | _: NullPointerException => println("OK!")
             |}
             |try {
             |  println("test")
             |} catch {
             |  case e@(_: IndexOutOfBoundsException | _: NullPointerException) => println("OK!")
             |}
             |""".stripMargin,
    )
  }

  test("catching Throwable with pattern match should be rejected") {
    assertErrors(
      1,
      scala"""
             |try {
             |  println("test")
             |} catch {
             |  case _: IndexOutOfBoundsException | _: Throwable => println("Not OK!")
             |}
             |""".stripMargin,
    )
  }

  test("catching Throwable using custom handler should be allowed") {
    assertNoErrors(scala"""
                          |object CustomHandler {
                          |  def apply[T](): PartialFunction[Throwable, T] = ???
                          |}
                          |
                          |try {
                          |  println("test")
                          |} catch CustomHandler()
                          |""".stripMargin)
  }
}
