package com.avsystem.commons
package serialization.cbor

import com.avsystem.commons.serialization.{GenCodec, TypeMarker}

final case class RawCbor(bytes: Array[Byte], offset: Int, length: Int) {
  require(offset >= 0 && length >= 0 && offset + length <= bytes.length)

  def apply(idx: Int): Byte =
    if (idx < 0 || idx >= length) throw new IndexOutOfBoundsException(idx)
    else bytes(offset + idx)

  def createInput(fieldLabels: FieldLabels): CborInput =
    new CborInput(new CborReader(this), fieldLabels)

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

  implicit val codec: GenCodec[RawCbor] =
    GenCodec.nonNull(
      input => input.readCustom(RawCbor).getOrElse(RawCbor(input.readSimple().readBinary())),
      (output, cbor) => if (!output.writeCustom(RawCbor, cbor)) {
        output.writeSimple().writeBinary(cbor.compact.bytes)
      }
    )
}
