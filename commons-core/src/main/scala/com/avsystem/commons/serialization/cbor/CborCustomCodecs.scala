package com.avsystem.commons
package serialization.cbor

import com.avsystem.commons.jiop.JFactory
import com.avsystem.commons.serialization.GenCodec.OOOFieldsObjectCodec
import com.avsystem.commons.serialization._

import scala.collection.compat._

trait CborCustomCodecs {
  /**
    * Creates a `GenCodec` for map type that leverages CBOR's ability to write object keys of arbitrary type.
    * Map keys are not written as strings but are serialized into raw CBOR.
    * Therefore, this codec requires a `GenCodec` for key type rather than `GenKeyCodec` required by standard
    * map codec. If the underlying `Input` or `Output` is not a CBOR input/output then keys are serialized as
    * HEX strings of their raw CBOR representation.
    */
  implicit def cborMapCodec[M[X, Y] <: BMap[X, Y], K: GenCodec, V: GenCodec](
    implicit fac: Factory[(K, V), M[K, V]]
  ): GenObjectCodec[M[K, V]] = mkMapCodec(implicit keyCodec => GenCodec.mapCodec[M, K, V])

  implicit def cborJMapCodec[M[X, Y] <: JMap[X, Y], K: GenCodec, V: GenCodec](
    implicit fac: JFactory[(K, V), M[K, V]]
  ): GenObjectCodec[M[K, V]] = mkMapCodec(implicit keyCodec => GenCodec.jMapCodec[M, K, V])

  private def mkMapCodec[M[X, Y] <: AnyRef, K: GenCodec, V: GenCodec](mkStdCodec: GenKeyCodec[K] => GenObjectCodec[M[K, V]])(
    implicit fac: Factory[(K, V), M[K, V]]
  ): GenObjectCodec[M[K, V]] = {
    implicit val genKeyCodec: GenKeyCodec[K] = new GenKeyCodec[K] {
      def read(key: String): K = CborInput.read[K](RawCbor.fromHex(key))
      def write(value: K): String = CborOutput.write[K](value).toString
    }

    implicit val stdObjectCodec: GenObjectCodec[M[K, V]] = mkStdCodec(genKeyCodec)

    val cborKeyCodec = new CborKeyCodec {
      def writeFieldKey(fieldName: String, output: CborOutput): Unit =
        output.writeRawCbor(RawCbor.fromHex(fieldName))
      def readFieldKey(input: CborInput): String =
        input.readRawCbor().toString
    }

    cborRawKeysCodec[M[K, V]](cborKeyCodec)
  }

  def cborRawKeysCodec[T: GenObjectCodec](keyCodec: CborKeyCodec): GenObjectCodec[T] =
    new CborRawKeysCodec[T](GenObjectCodec[T], keyCodec)
}
object CborCustomCodecs extends CborCustomCodecs

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

  def size(value: T): Int = stdObjectCodec.size(value)
  def nullable: Boolean = stdObjectCodec.nullable
}
