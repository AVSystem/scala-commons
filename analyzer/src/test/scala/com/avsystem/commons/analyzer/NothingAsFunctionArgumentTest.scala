package com.avsystem.commons
package analyzer

import org.scalatest.funsuite.AnyFunSuite

final class NothingAsFunctionArgumentTest extends AnyFunSuite with AnalyzerTest {

  test("passing throw where function is expected should fail") {
    assertErrors(
      1,
      scala"""
             |def register(callback: Int => Unit): Unit = ()
             |register(throw new Exception("boom"))
             |""".stripMargin,
    )
  }

  test("passing a proper function should pass") {
    assertNoErrors(
      scala"""
             |def register(callback: Int => Unit): Unit = ()
             |register(x => println(x))
             |""".stripMargin,
    )
  }

  test("wrapping throw in a lambda should pass") {
    assertNoErrors(
      scala"""
             |def register(callback: Int => Unit): Unit = ()
             |register(_ => throw new Exception("boom"))
             |""".stripMargin,
    )
  }

  test("non-function parameter with Nothing type should pass") {
    assertNoErrors(
      scala"""
             |def accept(value: Any): Unit = ()
             |accept(throw new Exception("boom"))
             |""".stripMargin,
    )
  }

  test("multiple function params where one is Nothing should fail") {
    assertErrors(
      1,
      scala"""
             |def twoCallbacks(a: Int => Unit, b: String => Unit): Unit = ()
             |twoCallbacks(x => println(x), throw new Exception("boom"))
             |""".stripMargin,
    )
  }

  test("Function0 parameter with Nothing should fail") {
    assertErrors(
      1,
      scala"""
             |def lazy0(thunk: () => Int): Int = thunk()
             |lazy0(throw new Exception("boom"))
             |""".stripMargin,
    )
  }
}
