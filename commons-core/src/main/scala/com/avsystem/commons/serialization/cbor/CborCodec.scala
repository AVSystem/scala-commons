package com.avsystem.commons
package serialization.cbor

import com.avsystem.commons.jiop.JFactory
import com.avsystem.commons.meta.Fallback
import com.avsystem.commons.serialization.{GenCodec, TransparentWrapping}

import scala.collection.compat.Factory

/**
  * Similar to `GenCodec` but assumes usage of `CborOutput` and `CborInput` rather than generic `Output` and `Input`.
  * This way the codec may take full advantage of CBOR format, e.g. arbitrarily-typed object keys
  * (as compared to string-only keys supported by `GenCodec`).
  */
trait CborCodec[T] {
  def read(input: CborInput): T
  def write(output: CborOutput, value: T): Unit
}
object CborCodec extends LowPriorityCborCodecs {
  def apply[T](implicit codec: CborCodec[T]): CborCodec[T] = codec

  def read[T: CborCodec](input: CborInput): T =
    CborCodec[T].read(input)

  def write[T: CborCodec](output: CborOutput, value: T): Unit =
    CborCodec[T].write(output, value)

  /**
    * Specialized CBOR codec for maps that can write keys of any type
    * (regular `GenCodec` can only write string keys).
    */
  implicit def mapCodec[M[X, Y] <: BMap[X, Y], K: CborCodec, V: CborCodec](
    implicit fac: Factory[(K, V), M[K, V]]
  ): CborCodec[M[K, V]] = mkMapCodec(fac, _.iterator)

  implicit def jMapCodec[M[X, Y] <: JMap[X, Y], K: CborCodec, V: CborCodec](
    implicit fac: JFactory[(K, V), M[K, V]]
  ): CborCodec[M[K, V]] = mkMapCodec(fac, _.asScala.iterator)

  private def mkMapCodec[M[X, Y], K: CborCodec, V: CborCodec](
    factory: Factory[(K, V), M[K, V]], iterator: M[K, V] => Iterator[(K, V)]
  ): CborCodec[M[K, V]] = new CborCodec[M[K, V]] {
    def read(input: CborInput): M[K, V] = {
      val oi = input.readObject()
      val b = factory.newBuilder
      while (oi.hasNext) {
        val key = CborCodec[K].read(oi.nextKey())
        val value = CborCodec[V].read(oi.nextValue())
        b += ((key, value))
      }
      b.result()
    }

    def write(output: CborOutput, value: M[K, V]): Unit = {
      val oo = output.writeObject()
      iterator(value).foreach { case (k, v) =>
        CborCodec[K].write(oo.writeKey(), k)
        CborCodec[V].write(oo.writeValue(), v)
      }
      oo.finish()
    }
  }

  implicit def fromTransparentWrapping[R, T](
    implicit wrapping: TransparentWrapping[R, T], wrappedCodec: CborCodec[R]
  ): CborCodec[T] =
    new CborCodec[T] {
      def read(input: CborInput): T = wrapping.wrap(CborCodec[R].read(input))
      def write(output: CborOutput, value: T): Unit = CborCodec[R].write(output, wrapping.unwrap(value))
    }

  implicit def fromFallback[T](implicit fallback: Fallback[CborCodec[T]]): CborCodec[T] =
    fallback.value
}
trait LowPriorityCborCodecs { this: CborCodec.type =>
  implicit def fromGenCodec[T: GenCodec]: CborCodec[T] = new CborCodec[T] {
    def read(input: CborInput): T = GenCodec[T].read(input)
    def write(output: CborOutput, value: T): Unit = GenCodec[T].write(output, value)
  }
}
