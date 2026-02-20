package com.avsystem.commons
package analyzer

import org.scalatest.funsuite.AnyFunSuite

final class CheckBincompatTest extends AnyFunSuite with AnalyzerTest {

  // Test-local @bincompat annotation since commons-core is not on analyzer test classpath
  private val bincompatAnnotDef = """
                                    |package com.avsystem.commons.annotation
                                    |import scala.annotation.StaticAnnotation
                                    |class bincompat extends StaticAnnotation
                                    |""".stripMargin

  test("definitions of @bincompat annotated symbols should not be rejected") {
    assertNoErrors(
      bincompatAnnotDef + scala"""
                                 |import com.avsystem.commons.annotation.bincompat
                                 |
                                 |@bincompat class klass
                                 |
                                 |@bincompat object objekt {
                                 |  @bincompat def method: Int = 42
                                 |}
                                 |""".stripMargin,
    )
  }

  test("usage of @bincompat annotated symbols should be rejected") {
    assertErrors(
      3,
      bincompatAnnotDef + scala"""
                                 |import com.avsystem.commons.annotation.bincompat
                                 |
                                 |@bincompat class klass
                                 |
                                 |@bincompat object objekt
                                 |
                                 |object outer {
                                 |  @bincompat def method: Int = 42
                                 |}
                                 |
                                 |object test {
                                 |  println(objekt)
                                 |  println(new klass)
                                 |  println(outer.method)
                                 |}
                                 |""".stripMargin,
    )
  }
}
