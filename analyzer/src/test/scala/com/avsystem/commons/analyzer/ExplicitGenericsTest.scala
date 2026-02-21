package com.avsystem.commons
package analyzer

import org.scalatest.funsuite.AnyFunSuite

final class ExplicitGenericsTest extends AnyFunSuite with AnalyzerTest {

  // Only enable ExplicitGenerics at Warn level, disable all others to avoid interference
  override protected def pluginOptions: List[String] = List("AVSystemAnalyzer:-_", "AVSystemAnalyzer:explicitGenerics")

  test("inferred type arguments on @explicitGenerics method should warn") {
    assertWarnings(
      1,
      scala"""
             |import com.avsystem.commons.annotation.explicitGenerics
             |@explicitGenerics def myMethod[A](a: A): A = a
             |val result = myMethod(42)
             |""".stripMargin,
    )
  }

  test("explicit type arguments on @explicitGenerics method should not warn") {
    assertWarnings(
      0,
      scala"""
             |import com.avsystem.commons.annotation.explicitGenerics
             |@explicitGenerics def myMethod[A](a: A): A = a
             |val result = myMethod[Int](42)
             |""".stripMargin,
    )
  }

  test("inferred type arguments on non-annotated method should not warn") {
    assertWarnings(
      0,
      scala"""
             |def myMethod[A](a: A): A = a
             |val result = myMethod(42)
             |""".stripMargin,
    )
  }

  test("no-op when @explicitGenerics annotation not on classpath") {
    assertWarnings(
      0,
      scala"""
             |def myMethod[A](a: A): A = a
             |val result = myMethod(42)
             |""".stripMargin,
    )
  }

  test("multiple type parameters all inferred should warn") {
    assertWarnings(
      1,
      scala"""
             |import com.avsystem.commons.annotation.explicitGenerics
             |@explicitGenerics def twoParams[A, B](a: A, b: B): (A, B) = (a, b)
             |val result = twoParams(1, "hello")
             |""".stripMargin,
    )
  }

  test("multiple type parameters all explicit should not warn") {
    assertWarnings(
      0,
      scala"""
             |import com.avsystem.commons.annotation.explicitGenerics
             |@explicitGenerics def twoParams[A, B](a: A, b: B): (A, B) = (a, b)
             |val result = twoParams[Int, String](1, "hello")
             |""".stripMargin,
    )
  }
}
