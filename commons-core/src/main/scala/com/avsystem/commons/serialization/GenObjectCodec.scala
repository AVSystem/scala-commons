package com.avsystem.commons
package serialization

import com.avsystem.commons.misc.MacroGenerated

import scala.annotation.implicitNotFound

@implicitNotFound("No GenObjectCodec found for ${T}")
trait GenObjectCodec[T] extends GenCodec[T] {
  def readObject(input: ObjectInput): T
  def writeObject(output: ObjectOutput, value: T): Unit

  override def read(input: Input): T = {
    val oi = input.readObject()
    val res = readObject(oi)
    oi.skipRemaining()
    res
  }

  override def write(output: Output, value: T): Unit = {
    val oo = output.writeObject()
    writeObject(oo, value)
    oo.finish()
  }
}
object GenObjectCodec {
  def materialize[T]: GenObjectCodec[T] = macro macros.serialization.GenCodecMacros.materialize[T]

  def fromApplyUnapplyProvider[T](applyUnapplyProvider: Any): GenObjectCodec[T] =
  macro macros.serialization.GenCodecMacros.fromApplyUnapplyProvider[T]

  implicit def macroGeneratedCodec[C, T]: MacroGenerated[C, GenObjectCodec[T]] =
  macro macros.serialization.GenCodecMacros.materializeMacroGenerated[T]
}
