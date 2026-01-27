package com.avsystem.commons
package serialization.cbor

import com.avsystem.commons.misc.Bytes
import com.avsystem.commons.serialization.SizePolicy
import org.scalatest.funsuite.AnyFunSuite

import java.io.{ByteArrayOutputStream, DataOutputStream}

class CborNonStringKeysTest extends AnyFunSuite {
  test("writing and reading CBOR map with non-string keys") {
    val baos = new ByteArrayOutputStream
    val output = new CborOutput(new DataOutputStream(baos), CborKeyCodec.Default, SizePolicy.Optional)

    val objout = output.writeObject()
    objout.declareSize(2)
    objout.writeKey().writeInt(42)
    objout.writeValue().writeString("42")
    objout.writeKey().writeBoolean(true)
    objout.writeValue().writeDouble(3.14)
    objout.finish()

    val bytes = baos.toByteArray
    assert(Bytes(bytes).toString == "A2182A623432F5FB40091EB851EB851F")

    val input = new CborInput(new CborReader(RawCbor(bytes)), CborKeyCodec.Default)
    val objin = input.readObject()
    assert(objin.nextKey().readInt() == 42)
    assert(objin.nextValue().readString() == "42")
    assert(objin.nextKey().readBoolean() == true)
    assert(objin.nextValue().readDouble() == 3.14)
    assert(!objin.hasNext)
  }
}
