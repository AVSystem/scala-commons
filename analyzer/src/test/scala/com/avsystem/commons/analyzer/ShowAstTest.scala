package com.avsystem.commons
package analyzer

import org.scalatest.funsuite.AnyFunSuite

final class ShowAstTest extends AnyFunSuite with AnalyzerTest {

  test("@showAst on val should emit AST as error") {
    assertErrors(
      1,
      scala"""
             |import com.avsystem.commons.annotation.showAst
             |@showAst val x: List[Int] = List(1, 2, 3)
             |""".stripMargin,
    )
  }

  test("@showAst on def should emit AST as error") {
    assertErrors(
      1,
      scala"""
             |import com.avsystem.commons.annotation.showAst
             |@showAst def foo(x: Int): Int = x + 1
             |""".stripMargin,
    )
  }

  test("@showAst on class should emit AST as error") {
    assertErrors(
      1,
      scala"""
             |import com.avsystem.commons.annotation.showAst
             |@showAst class Foo
             |""".stripMargin,
    )
  }

  test("val without @showAst should not emit error") {
    assertNoErrors(
      scala"""
             |val x: List[Int] = List(1, 2, 3)
             |""".stripMargin,
    )
  }

  test("def without @showAst should not emit error") {
    assertNoErrors(
      scala"""
             |def foo(x: Int): Int = x + 1
             |""".stripMargin,
    )
  }

  test("no-op when @showAst annotation not on classpath") {
    assertNoErrors(
      scala"""
             |val x: List[Int] = List(1, 2, 3)
             |def foo(x: Int): Int = x + 1
             |""".stripMargin,
    )
  }
}
