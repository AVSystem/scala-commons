package com.avsystem.commons
package analyzer

import org.scalatest.funsuite.AnyFunSuite

final class ImplicitValueClassesSuite extends AnyFunSuite with AnalyzerTest {
  test("implicit final class extending AnyVal should pass") {
    assertNoErrors(
      scala"""
             |implicit final class GoodImplicitClass(val x: Int) extends AnyVal {
             |  def double: Int = x * 2
             |}
             |""".stripMargin)
  }

  test("implicit class not extending AnyVal should fail") {
    assertErrors(1,
      scala"""
             |implicit final class BadImplicitClass(val x: Int) {
             |  def double: Int = x * 2
             |}
             |""".stripMargin)
  }

  test("implicit class with type parameter not extending AnyVal should fail") {
    assertErrors(1,
      scala"""
             |implicit final class BadImplicitClass[T <: Int](val x: T) {
             |  def double: Int = x * 2
             |}
             |""".stripMargin)
  }

  test("regular class should not be affected") {
    assertNoErrors(
      scala"""
             |class RegularClass(val x: Int) {
             |  def double: Int = x * 2
             |}
             |""".stripMargin)
  }

  test("implicit class with implicit parameter should not be affected") {
    assertNoErrors(
      scala"""
             |implicit final class ImplicitClassWithImplicitParameter(val x: Int)(implicit dummy: DummyImplicit) {
             |  def double: Int = x * 2
             |}
             |""".stripMargin)
  }

  test("implicit class extending other classes should not be affected") {
    assertNoErrors(
      scala"""
             |class SomeClass
             |
             |implicit final class GoodImplicitClass1(val x: Int) extends SomeClass {
             |  def double: Int = x * 2
             |}
             |
             |trait SomeTrait
             |implicit final class GoodImplicitClass2(val x: Int) extends SomeTrait {
             |  def double: Int = x * 2
             |}
             |""".stripMargin)
  }

  test("implicit class extending AnyVal with traits should be handled correctly") {
    assertErrors(1,
      scala"""
             |trait AnyTrait extends Any
             |implicit final class GoodImplicitClass(val x: Int) extends AnyVal with AnyTrait {
             |  def double: Int = x * 2
             |}
             |implicit final class BadImplicitClass(val x: Int) extends AnyTrait {
             |  def double: Int = x * 2
             |}
             |""".stripMargin)
  }

  test("nested implicit class not extending AnyVal should pass") {
    assertNoErrors(
      scala"""
             |class Outer {
             |  implicit final class NestedImplicitClass(val x: Int) {
             |    def double: Int = x * 2
             |  }
             |}
             |""".stripMargin)
  }

  test("implicit class for value class should not be affected") {
    assertNoErrors(
      scala"""
             |implicit final class ValueClass(x: com.avsystem.commons.misc.Timestamp) {
             |  def sth: Long = x.millis
             |}
             |""".stripMargin)
  }
}

final class NestedImplicitValueClassesSuite extends AnyFunSuite with AnalyzerTest {
  settings.pluginOptions.value ++= List("AVSystemAnalyzer:+implicitValueClasses:all")

  test("nested implicit class not extending AnyVal should fail") {
    assertErrors(1,
      scala"""
             |class Outer {
             |  implicit final class GoodNestedImplicitClass(val x: Int) {
             |    def double: Int = x * 2
             |  }
             |}
             |""".stripMargin)
  }


  test("nested implicit class with type parameter not extending AnyVal should fail") {
    assertErrors(1,
      scala"""
             |class Outer {
             | implicit final class BadNestedImplicitClass[T <: Int](val x: T) {
             |   def double: Int = x * 2
             | }
             |}
             |""".stripMargin)
  }

  test("deeply nested implicit class not extending AnyVal should fail") {
    assertErrors(1,
      scala"""
             |class Outer {
             | class Inner {
             |  implicit final class BadDeeplyNestedImplicitClass(val x: Int) {
             |    def double: Int = x * 2
             |  }
             | }
             |}
             |""".stripMargin)
  }

  test("regular class should not be affected") {
    assertNoErrors(
      scala"""
             |class RegularClass(val x: Int) {
             | def double: Int = x * 2
             |}
             |""".stripMargin)
  }

  test("implicit class extending other classes should not be affected") {
    assertNoErrors(
      scala"""
             |class Outer {
             | class SomeClass
             |
             | implicit final class GoodImplicitClass1(val x: Int) extends SomeClass {
             |   def double: Int = x * 2
             | }
             |
             | trait SomeTrait
             | implicit final class GoodImplicitClass2(val x: Int) extends SomeTrait {
             |   def double: Int = x * 2
             | }
             |}
             |""".stripMargin)
  }

  test("implicit class extending AnyVal with traits should be handled correctly") {
    assertErrors(1,
      scala"""
             |class Outer {
             | trait AnyTrait extends Any
             | implicit final class GoodImplicitClass(val x: Int) extends AnyVal with AnyTrait {
             |    def double: Int = x * 2
             | }
             | implicit final class BadImplicitClass(val x: Int) extends AnyTrait {
             |    def double: Int = x * 2
             | }
             |}
             |""".stripMargin)
  }
}
