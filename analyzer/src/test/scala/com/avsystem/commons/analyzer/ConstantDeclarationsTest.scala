package com.avsystem.commons
package analyzer


import org.scalatest.funsuite.AnyFunSuite

final class ConstantDeclarationsTest extends AnyFunSuite with AnalyzerTest {
  test("literal-valued constants should be non-lazy final vals with UpperCamelCase and no type annotation") {
    assertErrors(4,
      scala"""
             |// bad
             |val a = 10
             |val B = 10
             |final val c = 10
             |final val D: Int = 10
             |
             |// good
             |final val E = 10
             |""".stripMargin)
  }

  test("effectively final, non-literal UpperCamelCase vals should be final") {
    assertErrors(1,
      scala"""
             |// bad
             |val A = "foo".trim
             |
             |// good
             |final val B = "foo".trim
             |val c = "foo".trim
             |""".stripMargin)
  }

  test("no constant checking in traits or non-final classes") {
    assertNoErrors(
      scala"""
             |trait Whatever {
             |  val a = 10
             |  val B = 10
             |  final val c = 10
             |  final val D: Int = 10
             |  val A = "foo".trim
             |}
             |
             |class Stuff {
             |  val a = 10
             |  val B = 10
             |  final val c = 10
             |  final val D: Int = 10
             |  val A = "foo".trim
             |}
             |""".stripMargin)
  }

  test("no constant checking for overrides") {
    assertNoErrors(
      scala"""
             |trait Whatever {
             |  def a: Int
             |}
             |
             |object Stuff extends Whatever {
             |  val a: Int = 42
             |}
             |""".stripMargin)
  }

  test("no constant checking for privates") {
    assertNoErrors(
      scala"""
             |private val a = 10
             |private val B = 10
             |private final val c = 10
             |private final val D: Int = 10
             |private val A = "foo".trim
             |""".stripMargin)
  }
}
