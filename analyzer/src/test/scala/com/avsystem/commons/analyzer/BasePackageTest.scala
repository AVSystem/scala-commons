package com.avsystem.commons
package analyzer

import org.scalatest.funsuite.AnyFunSuite

final class BasePackageTest extends AnyFunSuite with AnalyzerTest {
  settings.pluginOptions.value ++= List("AVSystemAnalyzer:+basePackage:com.avsystem.commons")

  test("base package only") {
    //language=Scala
    assertNoErrors(
      """
        |package com.avsystem.commons
        |
        |object bar
        |""".stripMargin)
  }

  test("chained base package") {
    assertNoErrors(
      //language=Scala
      """
        |package com.avsystem
        |package commons
        |
        |object bar
        |""".stripMargin)
  }

  test("base package with chained subpackage") {
    assertNoErrors(
      //language=Scala
      """
        |package com.avsystem.commons
        |package core
        |
        |object bar
        |""".stripMargin)
  }

  test("base package object") {
    assertNoErrors(
      //language=Scala
      """
        |package com.avsystem
        |
        |package object commons
        |""".stripMargin)
  }

  test("base package object with imports") {
    assertNoErrors(
      //language=Scala
      """
        |package com.avsystem
        |
        |import scala.collection.mutable.Seq
        |import scala.collection.mutable.Set
        |
        |package object commons
        |""".stripMargin)
  }

  test("no base package") {
    assertErrors(1,
      //language=Scala
      """
        |object bar
        |""".stripMargin)
  }

  test("no base package with imports") {
    assertErrors(1,
      //language=Scala
      """
        |import scala.collection.mutable.Seq
        |import scala.collection.mutable.Set
        |
        |object bar
        |""".stripMargin)
  }


  test("wrong base package") {
    assertErrors(1,
      //language=Scala
      """
        |package com.avsystem.kommons
        |
        |object bar
        |""".stripMargin)
  }

  test("unchained subpackage") {
    assertErrors(1,
      //language=Scala
      """
        |package com.avsystem.commons.core
        |
        |object bar
        |""".stripMargin)
  }

  test("unchained subpackage with imports") {
    assertErrors(1,
      //language=Scala
      """
        |package com.avsystem.commons.core
        |
        |import scala.collection.mutable.Seq
        |import scala.collection.mutable.Set
        |
        |object bar
        |""".stripMargin)
  }
}
