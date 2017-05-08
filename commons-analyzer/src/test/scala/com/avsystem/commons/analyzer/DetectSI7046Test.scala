package com.avsystem.commons
package analyzer

import org.scalatest.FunSuite

class DetectSI7046Test extends FunSuite with AnalyzerTest {
  test("code in which SI-7046 occurs should be rejected") {
    assertErrors(
      """
        |import com.avsystem.commons.misc.SealedEnumCompanion
        |
        |sealed trait SomeEnum
        |object SomeEnum extends SealedEnumCompanion[SomeEnum] {
        |  case object First extends SomeEnum
        |  case object Second extends SomeEnum
        |  case object Third extends SomeEnum
        |
        |  val values: List[SomeEnum] = caseObjects
        |
        |  case object Fourth extends SomeEnum
        |}
      """.stripMargin
    )
  }
}
