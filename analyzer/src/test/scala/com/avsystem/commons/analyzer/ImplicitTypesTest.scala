package com.avsystem.commons
package analyzer

import org.scalatest.funsuite.AnyFunSuite

final class ImplicitTypesTest extends AnyFunSuite with AnalyzerTest:

  test("implicit definitions without explicit types should be rejected") {
    assertErrors(
      2,
      """
        |object whatever {
        |  implicit val x = 5
        |  implicit val y: Int = 5
        |  implicit def conv(x: Int) = x.toString
        |  implicit def conv2(x: Int): String = x.toString
        |  implicit object objekt
        |  implicit class wtf(x: Int)
        |}
      """.stripMargin
    )
  }

end ImplicitTypesTest
