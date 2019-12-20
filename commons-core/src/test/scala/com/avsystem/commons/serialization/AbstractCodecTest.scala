package com.avsystem.commons
package serialization

import org.scalactic.source.Position
import org.scalatest.funsuite.AnyFunSuite

trait AbstractCodecTest extends AnyFunSuite {
  def assertSameTypeValue[T](v1: T, v2: T)(implicit pos: Position): Unit = {
    assert(v1 == v2)
    assert(v1 == null || v1.getClass == v2.getClass)
  }

  type Raw

  def writeToOutput(write: Output => Unit): Raw
  def createInput(raw: Raw): Input

  def testWrite[T: GenCodec](value: T, expectedRepr: Raw)(implicit pos: Position): Unit = {
    val repr = writeToOutput(GenCodec.write[T](_, value))
    assert(repr == expectedRepr)
  }

  def testRead[T: GenCodec](repr: Raw, expected: T)(implicit pos: Position): Unit = {
    assert(expected == GenCodec.read[T](createInput(repr)))
  }

  def testRoundtrip[T: GenCodec](value: T)(implicit pos: Position): Unit = {
    val written: Raw = writeToOutput(GenCodec.write[T](_, value))
    val readBack = GenCodec.read[T](createInput(written))
    assertSameTypeValue(value, readBack)
  }
}
