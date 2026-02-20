package com.avsystem.commons
package analyzer

import org.scalatest.funsuite.AnyFunSuite

final class ConstantDeclarationsTest extends AnyFunSuite with AnalyzerTest {

  override protected def pluginOptions: List[String] = List("AVSystemAnalyzer:+constantDeclarations")

  test("final val with UpperCamelCase and literal value should pass") {
    assertNoErrors(
      scala"""
             |final val FooBar = 42
             |""".stripMargin,
    )
  }

  test("val with lowercase name and literal value should fail") {
    assertErrors(
      1,
      scala"""
             |val fooBar = 42
             |""".stripMargin,
    )
  }

  test("val with UpperCamelCase without final should fail") {
    assertErrors(
      1,
      scala"""
             |val FooBar = 42
             |""".stripMargin,
    )
  }

  test("final val with explicit type annotation on literal should fail") {
    assertErrors(
      1,
      scala"""
             |final val FooBar: Int = 42
             |""".stripMargin,
    )
  }

  test("final val with lowercase name and literal value should fail") {
    assertErrors(
      1,
      scala"""
             |final val fooBar = 42
             |""".stripMargin,
    )
  }

  test("val with non-constant value should pass") {
    assertNoErrors(
      scala"""
             |def someMethod(): Int = 42
             |val fooBar = someMethod()
             |""".stripMargin,
    )
  }

  test("final val with UpperCamelCase and non-constant value should pass") {
    assertNoErrors(
      scala"""
             |def someMethod(): Int = 42
             |final val FooBar = someMethod()
             |""".stripMargin,
    )
  }

  test("private val with literal value should pass") {
    assertNoErrors(
      scala"""
             |private val fooBar = 42
             |""".stripMargin,
    )
  }
}
