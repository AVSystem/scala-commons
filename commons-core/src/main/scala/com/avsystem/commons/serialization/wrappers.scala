package com.avsystem.commons
package serialization

import com.avsystem.commons.serialization.GenCodec.{ReadFailure, WriteFailure}

final class ObjectInputAsInput(objectInput: ObjectInput) extends Input {
  private def fail(expected: String): Nothing =
    throw new ReadFailure(s"expected $expected, got object")

  def isNull: Boolean = false
  def readNull(): Null = fail("null")
  def readString(): String = fail("string")
  def readBoolean(): Boolean = fail("boolean")
  def readInt(): Int = fail("int")
  def readLong(): Long = fail("long")
  def readDouble(): Double = fail("double")
  def readBigInt(): BigInt = fail("bigInt")
  def readBigDecimal(): BigDecimal = fail("bigDecimal")
  def readBinary(): Array[Byte] = fail("binary")
  def readList(): ListInput = fail("list")
  def readObject(): ObjectInput = objectInput
  def skip(): Unit = objectInput.skipRemaining()
}

final class ObjectOutputAsOutput(objectOutput: ObjectOutput, forwardFinish: Boolean) extends Output {
  private def fail(what: String): Nothing =
    throw new WriteFailure(s"could not write $what, can write only object")

  def writeNull(): Unit = fail("null")
  def writeString(str: String): Unit = fail("string")
  def writeBoolean(boolean: Boolean): Unit = fail("boolean")
  def writeInt(int: Int): Unit = fail("int")
  def writeLong(long: Long): Unit = fail("long")
  def writeDouble(double: Double): Unit = fail("double")
  def writeBigInt(bigInt: BigInt): Unit = fail("bigInt")
  def writeBigDecimal(bigDecimal: BigDecimal): Unit = fail("bigDecimal")
  def writeBinary(binary: Array[Byte]): Unit = fail("binary")
  def writeList(): ListOutput = fail("list")
  def writeObject(): ObjectOutput =
    if (forwardFinish) objectOutput
    else new ObjectOutput {
      def writeField(key: String): Output = objectOutput.writeField(key)
      def finish(): Unit = ()
    }
}
