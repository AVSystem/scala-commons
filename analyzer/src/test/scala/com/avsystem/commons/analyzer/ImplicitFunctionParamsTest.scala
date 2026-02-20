package com.avsystem.commons
package analyzer

import org.scalatest.funsuite.AnyFunSuite

final class ImplicitFunctionParamsTest extends AnyFunSuite with AnalyzerTest {

  test("implicit parameter with function type should fail") {
    assertErrors(
      1,
      scala"""
             |def foo(implicit f: Int => String): String = f(1)
             |""".stripMargin,
    )
  }

  test("using parameter with function type should fail") {
    assertErrors(
      1,
      scala"""
             |def foo(using f: Int => String): String = f(1)
             |""".stripMargin,
    )
  }

  test("using parameter with context function type should fail") {
    assertErrors(
      1,
      scala"""
             |def foo(using f: Int ?=> String): String = f(using 1)
             |""".stripMargin,
    )
  }

  test("implicit parameter with PartialFunction type should fail") {
    assertErrors(
      1,
      scala"""
             |def foo(implicit f: PartialFunction[Int, String]): String = f(1)
             |""".stripMargin,
    )
  }

  test("non-implicit parameter with function type should pass") {
    assertNoErrors(
      scala"""
             |def foo(f: Int => String): String = f(1)
             |""".stripMargin,
    )
  }

  test("implicit parameter with non-function type should pass") {
    assertNoErrors(
      scala"""
             |def foo(implicit n: Int): String = n.toString
             |""".stripMargin,
    )
  }

  test("multiple implicit function params should produce multiple errors") {
    assertErrors(
      2,
      scala"""
             |def foo(implicit f: Int => String, g: String => Int): Unit = ()
             |""".stripMargin,
    )
  }
}
