package com.avsystem.commons
package serialization.cbor

import com.avsystem.commons.misc.Bytes
import com.avsystem.commons.serialization.SizePolicy
import org.scalatest.funsuite.AnyFunSuite

import java.io.{ByteArrayOutputStream, DataOutputStream}
import java.nio.charset.StandardCharsets

class CborChunkedTest extends AnyFunSuite {
  test("chunked string input/output") {
    val chunks = List("foo", "dafuq", "baoz")

    val baos = new ByteArrayOutputStream
    val out = new CborOutput(new DataOutputStream(baos), CborKeyCodec.Default, SizePolicy.Optional)

    val chout = out.writeChunkedString()
    chunks.foreach(chout.writeChunk)
    chout.finish()
    val bytes = baos.toByteArray

    assert(Bytes(bytes).toString == "7F63666F6F6564616675716462616F7AFF")

    val input = new CborInput(new CborReader(RawCbor(bytes)), CborKeyCodec.Default)
    val chin = input.readChunkedString()
    val buf = new MListBuffer[String]
    while(chin.hasNext) {
      buf += chin.readChunk()
    }
    assert(buf.result() == chunks)
  }

  test("chunked binary input/output") {
    val chunks = List("foo", "dafuq", "baoz").map(_.getBytes(StandardCharsets.UTF_8))

    val baos = new ByteArrayOutputStream
    val out = new CborOutput(new DataOutputStream(baos), CborKeyCodec.Default, SizePolicy.Optional)

    val chout = out.writeChunkedBinary()
    chunks.foreach(chout.writeChunk)
    chout.finish()
    val bytes = baos.toByteArray

    assert(Bytes(bytes).toString == "5F43666F6F4564616675714462616F7AFF")

    val input = new CborInput(new CborReader(RawCbor(bytes)), CborKeyCodec.Default)
    val chin = input.readChunkedBinary()
    val buf = new MListBuffer[Array[Byte]]
    while(chin.hasNext) {
      buf += chin.readChunk()
    }
    assert(buf.result().map(Bytes(_)) == chunks.map(Bytes(_)))
  }
}
