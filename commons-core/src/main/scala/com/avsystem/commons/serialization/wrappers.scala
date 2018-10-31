package com.avsystem.commons
package serialization

import com.avsystem.commons.serialization.GenCodec.{ReadFailure, WriteFailure}

final class ObjectInputAsInput(objectInput: ObjectInput) extends Input {
  private def fail(expected: String): Nothing =
    throw new ReadFailure(s"expected $expected, got object")

  def readNull(): Boolean = false
  def readSimple(): SimpleInput = fail("simple value")
  def readList(): ListInput = fail("list")
  def readObject(): ObjectInput = objectInput

  def skip(): Unit = objectInput.skipRemaining()
}

final class ObjectOutputAsOutput(objectOutput: ObjectOutput, forwardFinish: Boolean) extends Output {
  private def fail(what: String): Nothing =
    throw new WriteFailure(s"could not write $what, can write only object")

  def writeNull(): Unit = fail("null")
  def writeSimple(): SimpleOutput = fail("simple value")
  def writeList(): ListOutput = fail("list")
  def writeObject(): ObjectOutput =
    if (forwardFinish) objectOutput
    else new ObjectOutput {
      def writeField(key: String): Output = objectOutput.writeField(key)
      def finish(): Unit = ()
    }
}
