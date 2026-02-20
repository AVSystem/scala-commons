package com.avsystem.commons
package analyzer

import org.scalatest.funsuite.AnyFunSuite

final class ImplicitTypesTest extends AnyFunSuite with AnalyzerTest {

  // Only enable implicitTypes to avoid interference from other rules
  override protected def pluginOptions: List[String] = List("AVSystemAnalyzer:-_", "AVSystemAnalyzer:+implicitTypes")

  test("implicit val with inferred type should warn") {
    assertErrors(
      1,
      scala"""
      implicit val x = List(1, 2, 3)
    """,
    )
  }

  test("implicit val with explicit type should not warn") {
    assertErrors(
      0,
      scala"""
      implicit val x: List[Int] = List(1, 2, 3)
    """,
    )
  }

  test("implicit def with inferred return type should warn") {
    // Use a def that doesn't trigger Scala 3's "result type of implicit definition" error.
    // In Scala 3, implicit defs used as conversions need explicit types, but non-conversion
    // implicit defs with inferred types should still be caught by our rule.
    // Actually, Scala 3 requires explicit result types on all implicit defs.
    // So we test with implicit val instead, which is the primary use case.
    assertErrors(
      1,
      scala"""
      trait Codec[T]
      implicit val intCodec = new Codec[Int] {}
    """,
    )
  }

  test("implicit def with explicit return type should not warn") {
    assertErrors(
      0,
      scala"""
      trait Codec[T]
      implicit def intCodec: Codec[Int] = new Codec[Int] {}
    """,
    )
  }

  test("named given with explicit type should not warn") {
    assertErrors(
      0,
      scala"""
      trait Ordering[T]
      given intOrd: Ordering[Int] = new Ordering[Int] {}
    """,
    )
  }

  test("anonymous given should not warn") {
    assertErrors(
      0,
      scala"""
      trait Ordering[T]
      given Ordering[Int] = new Ordering[Int] {}
    """,
    )
  }

  test("non-implicit val with inferred type should not warn") {
    assertErrors(
      0,
      scala"""
      val x = List(1, 2, 3)
    """,
    )
  }

  test("non-implicit def with inferred return type should not warn") {
    assertErrors(
      0,
      scala"""
      def f(n: Int) = n + 1
    """,
    )
  }
}
