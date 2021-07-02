package com.avsystem.commons
package serialization.cbor

import com.avsystem.commons.serialization.{GenCodec, SizePolicy, TypeMarker}

import scala.annotation.tailrec

final case class RawCbor(bytes: Array[Byte], offset: Int, length: Int) {
  require(offset >= 0 && length >= 0 && offset + length <= bytes.length)

  override def equals(other: Any): Boolean = other match {
    case RawCbor(otherBytes, otherOffset, otherLength) =>
      java.util.Arrays.equals(
        bytes, offset, offset + length,
        otherBytes, otherOffset, otherOffset + otherLength
      )
  }

  override lazy val hashCode: Int = {
    @tailrec def loop(i: Int, acc: Int): Int =
      if (i >= offset + length) acc
      else loop(i + 1, acc * 31 + bytes(i))
    loop(offset, 1)
  }

  override def toString: String =
    (offset until offset + length).map(i => f"${bytes(i) & 0xFF}%02X").mkString

  def apply(idx: Int): Byte =
    if (idx < 0 || idx >= length) throw new IndexOutOfBoundsException
    else bytes(offset + idx)

  def createInput(keyCodec: CborKeyCodec = CborKeyCodec.Default): CborInput =
    new CborInput(new CborReader(this), keyCodec)

  def readAs[T: GenCodec](keyCodec: CborKeyCodec = CborKeyCodec.Default): T =
    GenCodec.read[T](createInput(keyCodec))

  def safeCopy: RawCbor = {
    val newBytes = new Array[Byte](length)
    System.arraycopy(bytes, offset, newBytes, 0, length)
    RawCbor(newBytes)
  }

  def compact: RawCbor =
    if (offset == 0 && length == bytes.length) this
    else safeCopy
}
object RawCbor extends TypeMarker[RawCbor] {
  final val Empty = RawCbor(Array.empty)

  def apply(bytes: Array[Byte]): RawCbor =
    RawCbor(bytes, 0, bytes.length)

  def fromHex(hex: String): RawCbor =
    RawCbor(hex.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray)

  def write[T: GenCodec](
    value: T,
    keyCodec: CborKeyCodec = CborKeyCodec.Default,
    sizePolicy: SizePolicy = SizePolicy.Optional
  ): RawCbor =
    RawCbor(CborOutput.write(value, keyCodec, sizePolicy))

  implicit val codec: GenCodec[RawCbor] =
    GenCodec.nonNull(
      input => input.readCustom(RawCbor).getOrElse(RawCbor(input.readSimple().readBinary())),
      (output, cbor) => if (!output.writeCustom(RawCbor, cbor)) {
        output.writeSimple().writeBinary(cbor.compact.bytes)
      }
    )
}
