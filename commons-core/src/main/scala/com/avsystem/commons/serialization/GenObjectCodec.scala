package com.avsystem.commons
package serialization

import com.avsystem.commons.derivation.DeferredInstance
import com.avsystem.commons.misc.MacroGenerated

import scala.annotation.implicitNotFound

/**
  * Subtype of [[GenCodec]] which captures serialization to an "object", i.e. through [[ObjectOutput]] and
  * [[ObjectInput]].
  */
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
  def apply[T](implicit codec: GenObjectCodec[T]): GenObjectCodec[T] = codec

  def writeObject[T: GenObjectCodec](output: ObjectOutput, value: T): Unit =
    apply[T].writeObject(output, value)
  def readObject[T: GenObjectCodec](input: ObjectInput): T =
    apply[T].readObject(input)

  def materialize[T]: GenObjectCodec[T] = macro macros.serialization.GenCodecMacros.materialize[T]

  def fromApplyUnapplyProvider[T](applyUnapplyProvider: Any): GenObjectCodec[T] =
  macro macros.serialization.GenCodecMacros.fromApplyUnapplyProvider[T]

  def create[T](readFun: ObjectInput => T, writeFun: (ObjectOutput, T) => Any): GenObjectCodec[T] =
    new GenObjectCodec[T] {
      def readObject(input: ObjectInput): T = readFun(input)
      def writeObject(output: ObjectOutput, value: T): Unit = writeFun(output, value)
    }

  def transformed[T, R: GenObjectCodec](toRaw: T => R, fromRaw: R => T): GenObjectCodec[T] =
    new Transformed[T, R](GenObjectCodec[R], toRaw, fromRaw)

  final class Transformed[A, B](val wrapped: GenObjectCodec[B], onWrite: A => B, onRead: B => A)
    extends GenObjectCodec[A] {

    def readObject(input: ObjectInput): A = onRead(wrapped.readObject(input))
    def writeObject(output: ObjectOutput, value: A): Unit = wrapped.writeObject(output, onWrite(value))
  }

  final class Deferred[T] extends DeferredInstance[GenObjectCodec[T]] with GenObjectCodec[T] {
    def readObject(input: ObjectInput): T = underlying.readObject(input)
    def writeObject(output: ObjectOutput, value: T): Unit = underlying.writeObject(output, value)
  }

  implicit def macroGeneratedCodec[C, T]: MacroGenerated[C, GenObjectCodec[T]] =
  macro macros.serialization.GenCodecMacros.materializeMacroGenerated[T]
}
