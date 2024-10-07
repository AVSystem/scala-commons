package com.avsystem.commons
package analyzer

import org.scalatest.funsuite.AnyFunSuite

final class OldStyleImplicitConversionTest extends AnyFunSuite with AnalyzerTest:

  test("old-style implicit direct conversions should be reported") {
    assertErrors(
      1,
      """
        |object Whatever {
        |  implicit def intToString(i: Int): String = i.toString
        |}
      """.stripMargin
    )
  }

  test("old-style implicit class conversions should be reported") {
    assertErrors(
      2,
      """
        |object Whatever {
        |  implicit class IntOps(i: Int) {
        |    def toStr: String = i.toString
        |  }
        |  implicit class LongOps(val l: Long) extends AnyVal {
        |    def toStr: String = l.toString
        |  }
        |}
      """.stripMargin
    )
  }

  test("new-style implicit conversions should not be reported") {
    assertNoErrors(
      """
        |object Whatever {
        |  given intToString: Conversion[Int, String] = _.toString
        |  extension (i: Int)
        |     def toStr: String = i.toString
        |}
        """.stripMargin
    )
  }

end OldStyleImplicitConversionTest
