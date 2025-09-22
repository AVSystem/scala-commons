package com.avsystem.commons.serialization

import org.scalatest.funsuite.AnyFunSuite

final class NotUsedTransientDefault extends AnyFunSuite {

  test("no warnings when @transientDefault is used properly") {
    assertCompiles(
      //language=Scala
      s"""
         |case class X(@transientDefault a: String = "default")
         |val codec = GenCodec.materialize[X]
         |""".stripMargin
    )
  }

  test("warnings when missing default value") {
    assertDoesNotCompile(
      //language=Scala
      s"""
         |case class X(@transientDefault a: String)
         |val codec = GenCodec.materialize[X]
         |""".stripMargin
    )
  }
}
