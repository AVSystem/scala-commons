package com.avsystem.commons
package analyzer

import org.scalatest.funsuite.AnyFunSuite

final class VarargsAtLeastTest extends AnyFunSuite with AnalyzerTest {

  // Test-local @atLeast annotation since commons-core is not on analyzer test classpath
  private val atLeastAnnotDef = """
                                  |package com.avsystem.commons.annotation
                                  |import scala.annotation.StaticAnnotation
                                  |class atLeast(n: Int) extends StaticAnnotation
                                  |""".stripMargin

  private val testUtilsDef = """
                               |import com.avsystem.commons.annotation.atLeast
                               |object TestUtils {
                               |  def need3Params(@atLeast(3) args: Any*): Unit = ()
                               |}
                               |""".stripMargin

  test("too few varargs parameters should be rejected") {
    assertErrors(
      1,
      atLeastAnnotDef + scala"""
                               |$testUtilsDef
                               |TestUtils.need3Params(1, 2)
                               |""".stripMargin,
    )
  }

  test("enough varargs parameters should not be rejected") {
    assertNoErrors(
      atLeastAnnotDef + scala"""
                               |$testUtilsDef
                               |TestUtils.need3Params(1, 2, 3)
                               |TestUtils.need3Params(1, 2, 3, 4)
                               |""".stripMargin,
    )
  }

  test("collection passed as varargs parameter should not be rejected") {
    assertNoErrors(
      atLeastAnnotDef + scala"""
                               |$testUtilsDef
                               |TestUtils.need3Params(List(1, 2)*)
                               |""".stripMargin,
    )
  }
}
