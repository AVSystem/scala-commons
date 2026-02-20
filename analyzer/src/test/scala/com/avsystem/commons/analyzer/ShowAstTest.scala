package com.avsystem.commons
package analyzer

import org.scalatest.funsuite.AnyFunSuite

final class ShowAstTest extends AnyFunSuite with AnalyzerTest {

  // Test-local @showAst annotation since commons-core is not on analyzer test classpath
  private val showAstAnnotDef =
    """
      |package com.avsystem.commons.annotation
      |import scala.annotation.StaticAnnotation
      |class showAst extends StaticAnnotation
      |""".stripMargin

  test("@showAst on val should emit AST as error") {
    assertErrors(
      1,
      showAstAnnotDef + scala"""
                               |import com.avsystem.commons.annotation.showAst
                               |@showAst val x: List[Int] = List(1, 2, 3)
                               |""".stripMargin,
    )
  }

  test("@showAst on def should emit AST as error") {
    assertErrors(
      1,
      showAstAnnotDef + scala"""
                               |import com.avsystem.commons.annotation.showAst
                               |@showAst def foo(x: Int): Int = x + 1
                               |""".stripMargin,
    )
  }

  test("@showAst on class should emit AST as error") {
    assertErrors(
      1,
      showAstAnnotDef + scala"""
                               |import com.avsystem.commons.annotation.showAst
                               |@showAst class Foo
                               |""".stripMargin,
    )
  }

  test("val without @showAst should not emit error") {
    assertNoErrors(
      showAstAnnotDef + scala"""
                               |val x: List[Int] = List(1, 2, 3)
                               |""".stripMargin,
    )
  }

  test("def without @showAst should not emit error") {
    assertNoErrors(
      showAstAnnotDef + scala"""
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
