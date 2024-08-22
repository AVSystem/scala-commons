package com.avsystem.commons
package analyzer

import org.scalatest.funsuite.AnyFunSuite

final class ByNameParametersTest extends AnyFunSuite with AnalyzerTest {

  test("should report by-name implicit parameters") {
    assertErrors(2,
      //language=Scala
      """
        |object whatever {
        |  def byName(x: => Int) = x
        |  def byName(implicit x: => String) = x
        |  def byName(x: => Int)(implicit y: => Int) = x
        |  def byName(implicit x: Int) = x
        |}
      """.stripMargin
    )
  }

  test("should report by-name implicit constructor parameters") {
    assertErrors(2,
      //language=Scala
      """
        |object whatever {
        |  class C(x: => Int)(implicit y: => Int, z: String)
        |  class D(implicit y: => Int, z: => String)
        |}
        |""".stripMargin
    )
  }
}
