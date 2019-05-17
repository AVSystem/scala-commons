package com.avsystem.commons
package serialization.cbor

import com.avsystem.commons.serialization.GenCodec.ReadFailure
import com.avsystem.commons.serialization.TypeMarker

final case class RawCbor(bytes: Array[Byte], offset: Int, length: Int) {
  require(offset + length <= bytes.length)

  def apply(idx: Int): Byte =
    if (idx < 0) throw new IndexOutOfBoundsException
    else if (idx >= length) throw new ReadFailure("EOF")
    else bytes(offset + idx)

  def createInput(fieldLabels: FieldLabels): CborInput =
    new CborInput(new CborReader(this), fieldLabels)

  def safeCopy: RawCbor = {
    val newBytes = new Array[Byte](length)
    System.arraycopy(bytes, offset, newBytes, 0, length)
    RawCbor(newBytes)
  }
}
object RawCbor extends TypeMarker[RawCbor] {
  def apply(bytes: Array[Byte]): RawCbor =
    RawCbor(bytes, 0, bytes.length)
}
