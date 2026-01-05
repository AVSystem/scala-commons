package com.avsystem.commons.serialization

import org.scalatest.funsuite.AnyFunSuite

final class NotUsedTransientDefault extends AnyFunSuite {
  case class Valid(@transientDefault a: String = "default")
  case class Invalid(@transientDefault a: String)

  test("no warnings when @transientDefault is used properly") {
    assertCompiles(
      //language=Scala
      s"""
         |GenCodec.materialize[Valid]
         |""".stripMargin
    )
  }

  test("fails to compile when missing default value") {
    assertDoesNotCompile(
      //language=Scala
      s"""
         |GenCodec.materialize[Invalid]
         |""".stripMargin
    )
  }
}
