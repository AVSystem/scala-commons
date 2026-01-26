package com.avsystem.commons
package serialization

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream}

class StreamGenCodecTest extends GenCodecRoundtripTest {
  type Raw = Array[Byte]

  def writeToOutput(write: Output => Unit): Array[Byte] = {
    val baos = new ByteArrayOutputStream
    write(new StreamOutput(new DataOutputStream(baos)))
    baos.toByteArray
  }

  def createInput(raw: Array[Byte]): Input =
    new StreamInput(new DataInputStream(new ByteArrayInputStream(raw)))
}
