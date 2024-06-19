package com.avsystem.commons
package analyzer

import org.scalatest.funsuite.AnyFunSuite

final class RequireExplicitNothingTest extends AnyFunSuite with AnalyzerTest {
  settings.pluginOptions.value ++= List("AVSystemAnalyzer:-discardedMonixTask")

  test("simple") {
    assertErrors(4,
      //language=Scala
      """|
         |object Test {
         |  //not ok
         |  val nook1 = throw new Exception
         |  val nook2 = throw new Exception
         |  def nook3 = throw new Exception
         |  val nook4: List[Int] = List.empty //covariant
         |
         |  //ok
         |  val ok1: Nothing = throw new Exception
         |  val ok2: Nothing = throw new Exception
         |  def ok3: Nothing = throw new Exception
         |  val ok4: List[Nothing] = List.empty[Nothing]
         |  val ok5: List[Nothing] = Nil
         |  val ok6: List[Int] = Nil
         |  val ok7: Set[Int] = Set.empty //invariant
         |}
         |""".stripMargin
    )
  }
}
