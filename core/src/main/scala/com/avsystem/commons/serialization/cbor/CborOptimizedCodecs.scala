package com.avsystem.commons
package serialization.cbor

import com.avsystem.commons.jiop.JFactory
import com.avsystem.commons.serialization.GenCodec.OOOFieldsObjectCodec
import com.avsystem.commons.serialization._

import scala.collection.Factory

trait CborOptimizedCodecs {
  /**
    * Creates a `GenCodec` for map type that leverages CBOR's ability to write object keys of arbitrary type.
    * Map keys are not written as strings but are serialized into raw CBOR instead.
    * Therefore, this codec requires a `GenCodec` for key type rather than `GenKeyCodec` required by standard
    * map codec.
    *
    * If the underlying `Input` or `Output` is not a CBOR input/output and there is no `GenKeyCodec`
    * for key type then a fallback `GenKeyCodec` is used that converts keys into strings by taking HEX representation
    * of their CBOR serialization. If the key type has a `GenKeyCodec` then this `GenCodec` behaves exactly the same
    * as the standard one for non-CBOR inputs/outputs.
    */
  implicit def cborMapCodec[M[X, Y] <: BMap[X, Y], K: GenCodec : OptGenKeyCodec, V: GenCodec](
    implicit fac: Factory[(K, V), M[K, V]]
  ): GenObjectCodec[M[K, V]] = mkMapCodec(implicit keyCodec => GenCodec.mapCodec[M, K, V])

  implicit def cborJMapCodec[M[X, Y] <: JMap[X, Y], K: GenCodec : OptGenKeyCodec, V: GenCodec](
    implicit fac: JFactory[(K, V), M[K, V]]
  ): GenObjectCodec[M[K, V]] = mkMapCodec(implicit keyCodec => GenCodec.jMapCodec[M, K, V])

  private def mkMapCodec[M[X, Y] <: AnyRef, K: GenCodec : OptGenKeyCodec, V: GenCodec](
    mkStdCodec: GenKeyCodec[K] => GenObjectCodec[M[K, V]]
  )(implicit
    fac: Factory[(K, V), M[K, V]]
  ): GenObjectCodec[M[K, V]] = {
    val hexKeysStdCodec = mkStdCodec(new GenKeyCodec[K] {
      def read(key: String): K = CborInput.readRawCbor[K](RawCbor.fromHex(key))
      def write(value: K): String = CborOutput.writeRawCbor[K](value).toString
    })

    val regularStdCodec =
      OptGenKeyCodec[K].keyCodec.map(mkStdCodec).getOrElse(hexKeysStdCodec)

    val cborKeyCodec = new CborKeyCodec {
      def writeFieldKey(fieldName: String, output: CborOutput): Unit =
        output.writeRawCbor(RawCbor.fromHex(fieldName))
      def readFieldKey(input: CborInput): String =
        input.readRawCbor().toString
    }

    new GenObjectCodec[M[K, V]] {
      override def readObject(input: ObjectInput): M[K, V] =
        if (input.customEvent(ForceCborKeyCodec, cborKeyCodec))
          hexKeysStdCodec.readObject(input)
        else
          regularStdCodec.readObject(input)

      override def writeObject(output: ObjectOutput, value: M[K, V]): Unit =
        if (output.customEvent(ForceCborKeyCodec, cborKeyCodec))
          hexKeysStdCodec.writeObject(output, value)
        else
          regularStdCodec.writeObject(output, value)
    }
  }
}
object CborOptimizedCodecs extends CborOptimizedCodecs

class CborRawKeysCodec[T](stdObjectCodec: GenObjectCodec[T], keyCodec: CborKeyCodec) extends GenObjectCodec[T] {
  def readObject(input: ObjectInput): T = {
    input.customEvent(ForceCborKeyCodec, keyCodec)
    stdObjectCodec.readObject(input)
  }

  def writeObject(output: ObjectOutput, value: T): Unit = {
    output.customEvent(ForceCborKeyCodec, keyCodec)
    stdObjectCodec.writeObject(output, value)
  }
}

class OOOFieldCborRawKeysCodec[T](stdObjectCodec: OOOFieldsObjectCodec[T], keyCodec: CborKeyCodec) extends OOOFieldsObjectCodec[T] {
  def readObject(input: ObjectInput, outOfOrderFields: FieldValues): T = {
    input.customEvent(ForceCborKeyCodec, keyCodec)
    stdObjectCodec.readObject(input, outOfOrderFields)
  }

  def writeFields(output: ObjectOutput, value: T): Unit = {
    output.customEvent(ForceCborKeyCodec, keyCodec)
    stdObjectCodec.writeFields(output, value)
  }

  def size(value: T, output: Opt[SequentialOutput]): Int = stdObjectCodec.size(value, output)
  def nullable: Boolean = stdObjectCodec.nullable
}

/**
  * Makes sure that [[CborRawKeysCodec]] for map types behaves exactly the same for non-CBOR inputs/outputs as
  * standard `GenCodec` when the key type has a `GenKeyCodec`. However, when `GenKeyCodec` is not available for key
  * type, [[CborRawKeysCodec]] can still work with non-CBOR inputs/outputs by serializing keys into CBOR and taking
  * their HEX representation as standard string keys.
  */
case class OptGenKeyCodec[K](keyCodec: Opt[GenKeyCodec[K]])
object OptGenKeyCodec extends OptGenKeyCodecLowPriority {
  def apply[K](implicit optGenKeyCodec: OptGenKeyCodec[K]): OptGenKeyCodec[K] = optGenKeyCodec

  implicit def fromKeyCodec[K: GenKeyCodec]: OptGenKeyCodec[K] = OptGenKeyCodec(Opt(GenKeyCodec[K]))
}
trait OptGenKeyCodecLowPriority { this: OptGenKeyCodec.type =>
  implicit def noKeyCodec[K]: OptGenKeyCodec[K] = OptGenKeyCodec(Opt.Empty)
}
