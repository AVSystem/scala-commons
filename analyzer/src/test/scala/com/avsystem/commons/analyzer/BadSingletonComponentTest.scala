package com.avsystem.commons
package analyzer

import org.scalatest.funsuite.AnyFunSuite

final class BadSingletonComponentTest extends AnyFunSuite with AnalyzerTest:

  test("general") {
    assertErrors(
      5,
      """
        |import com.avsystem.commons.di._
        |
        |object test extends Components {
        |  singleton(123)
        |  val notDef = singleton(123)
        |  def hasParams(param: Int) = singleton(param)
        |  def hasTypeParams[T]: Component[T] = singleton(???)
        |  def outerMethod: Component[Int] = {
        |    def innerMethod = singleton(123)
        |    innerMethod
        |  }
        |
        |  def good: Component[Int] = singleton(123)
        |  def alsoGood: Component[Int] = { singleton(123) }
        |  def goodAsWell: Component[Int] = singleton(123).dependsOn(good)
        |}
      """.stripMargin
    )
  }

end BadSingletonComponentTest
