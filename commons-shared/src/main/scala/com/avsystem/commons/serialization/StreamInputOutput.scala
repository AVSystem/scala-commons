package com.avsystem.commons
package serialization


import java.io.{DataInputStream, DataOutputStream}
import java.{lang => jl}

import com.avsystem.commons.misc.Opt
import com.avsystem.commons.serialization.GenCodec.ReadFailure


private object FormatConstants {
  val ByteBytes = jl.Byte.BYTES
  val ShortBytes = jl.Short.BYTES
  val IntBytes = jl.Integer.BYTES
  val LongBytes = jl.Long.BYTES
  val FloatBytes = jl.Float.BYTES
  val DoubleBytes = jl.Double.BYTES

  val NullMarker: Byte = 0
  val StringMarker: Byte = 1
  val ByteMarker: Byte = 2
  val ShortMarker: Byte = 3
  val IntMarker: Byte = 4
  val LongMarker: Byte = 5
  val FloatMarker: Byte = 6
  val DoubleMarker: Byte = 7
  val ByteArrayMarker: Byte = 8
  val BooleanMarker: Byte = 9
  val ListStartMarker: Byte = 10
  val ObjectStartMarker: Byte = 11
  val ListEndMarker: Byte = 12
  val ObjectEndMarker: Byte = 13
}

import com.avsystem.commons.serialization.FormatConstants._

class StreamInput(is: DataInputStream) extends Input {
  private[serialization] val markerByte = is.readByte()

  def inputType: InputType = markerByte match {
    case NullMarker =>
      InputType.Null
    case ListStartMarker =>
      InputType.List
    case ObjectStartMarker =>
      InputType.Object
    case _ =>
      InputType.Simple
  }

  def readNull(): Null = if (markerByte == NullMarker)
    null
  else
    throw new ReadFailure(s"Expected null, but $markerByte found")

  def readString(): String = if (markerByte == StringMarker)
    is.readUTF()
  else
    throw new ReadFailure(s"Expected string, but $markerByte found")

  def readBoolean(): Boolean = if (markerByte == BooleanMarker)
    is.readBoolean()
  else
    throw new ReadFailure(s"Expected boolean, but $markerByte found")

  def readInt(): Int = if (markerByte == IntMarker)
    is.readInt()
  else
    throw new ReadFailure(s"Expected int, but $markerByte found")

  def readLong(): Long = if (markerByte == LongMarker)
    is.readLong()
  else
    throw new ReadFailure(s"Expected long, but $markerByte found")

  def readDouble(): Double = if (markerByte == DoubleMarker)
    is.readDouble()
  else
    throw new ReadFailure(s"Expected double, but $markerByte found")

  def readBinary(): Array[Byte] = if (markerByte == ByteArrayMarker) {
    val binary = Array.ofDim[Byte](is.readInt())
    is.readFully(binary)
    binary
  } else {
    throw new ReadFailure(s"Expected binary array, but $markerByte found")
  }

  def readList(): ListInput = if (markerByte == ListStartMarker)
    new StreamListInput(is)
  else
    throw new ReadFailure(s"Expected list, but $markerByte found")

  def readObject(): ObjectInput = if (markerByte == ObjectStartMarker)
    new StreamObjectInput(is)
  else
    throw new ReadFailure(s"Expected object, but $markerByte found")

  def skip(): Unit = {
    val toSkip = markerByte match {
      case NullMarker =>
      // no op
      case StringMarker =>
        val n = is.readUnsignedShort()
        is.skipBytes(n)
      case ByteMarker =>
        is.skipBytes(ByteBytes)
      case ShortMarker =>
        is.skipBytes(ShortBytes)
      case IntMarker =>
        is.skipBytes(IntBytes)
      case LongMarker =>
        is.skipBytes(LongBytes)
      case FloatMarker =>
        is.skipBytes(FloatBytes)
      case DoubleMarker =>
        is.skipBytes(DoubleBytes)
      case ByteArrayMarker =>
        is.skipBytes(is.readInt())
      case BooleanMarker =>
        is.skipBytes(ByteBytes)
      case ListStartMarker =>
        new StreamListInput(is).skipRemaining()
      case ObjectStartMarker =>
        new StreamObjectInput(is).skipRemaining()
      case unexpected =>
        throw new ReadFailure(s"Unexpected marker byte: $unexpected")
    }
  }
}

class StreamFieldInput(val fieldName: String, is: DataInputStream) extends StreamInput(is) with FieldInput

private class StreamListInput(is: DataInputStream) extends ListInput {
  private[this] var currentInput: Opt[StreamInput] = Opt.empty

  private def ensureInput(): Unit =
    if (currentInput == Opt.empty) currentInput = Opt.some(new StreamInput(is))

  def nextElement(): Input = {
    if (!hasNext) throw new ReadFailure("List already emptied")
    val input = currentInput
    currentInput = Opt.empty
    input.get
  }

  def hasNext: Boolean = {
    ensureInput()
    currentInput.get.markerByte != ListEndMarker
  }
}

private class StreamObjectInput(is: DataInputStream) extends ObjectInput {

  import StreamObjectInput._

  private[this] var currentField: FieldInput = NoneYet

  private def ensureInput(): Unit = {
    if (currentField eq NoneYet) {
      val keyInput = new StreamInput(is)
      currentField = if (keyInput.markerByte != ObjectEndMarker) {
        val keyString = keyInput.readString()
        new StreamFieldInput(keyString, is)
      } else {
        End
      }
    }
  }

  def nextField(): FieldInput = {
    if (!hasNext) throw new ReadFailure("Object already emptied")
    val field = currentField
    currentField = NoneYet
    field
  }

  def hasNext: Boolean = {
    ensureInput()
    currentField ne End
  }
}

private object StreamObjectInput {
  case class EmptyFieldInput(name: String) extends FieldInput {
    private def nope: Nothing = throw new ReadFailure(s"Something went horribly wrong ($name)")

    def fieldName: String = nope
    def inputType: InputType = nope
    def readNull(): Null = nope
    def readString(): String = nope
    def readBoolean(): Boolean = nope
    def readInt(): Int = nope
    def readLong(): Long = nope
    def readDouble(): Double = nope
    def readBinary(): Array[Byte] = nope
    def readList(): ListInput = nope
    def readObject(): ObjectInput = nope
    def skip(): Unit = nope
  }

  val NoneYet = EmptyFieldInput("NONE")
  val End = EmptyFieldInput("END")
}

class StreamOutput(os: DataOutputStream) extends Output {

  private[this] val streamList = new StreamListOutput(os, this)
  private[this] val streamObject = new StreamObjectOutput(os, this)

  def writeNull(): Unit = os.write(NullMarker)

  def writeString(str: String): Unit = {
    os.writeByte(StringMarker)
    os.writeUTF(str)
  }

  def writeBoolean(boolean: Boolean): Unit = {
    os.writeByte(BooleanMarker)
    os.writeBoolean(boolean)
  }

  def writeInt(int: Int): Unit = {
    os.writeByte(IntMarker)
    os.writeInt(int)
  }

  def writeLong(long: Long): Unit = {
    os.writeByte(LongMarker)
    os.writeLong(long)
  }

  def writeDouble(double: Double): Unit = {
    os.writeByte(DoubleMarker)
    os.writeDouble(double)
  }

  def writeBinary(binary: Array[Byte]): Unit = {
    os.writeByte(ByteArrayMarker)
    os.writeInt(binary.length)
    os.write(binary)
  }

  def writeList(): ListOutput = {
    os.writeByte(ListStartMarker)
    streamList
  }

  def writeObject(): ObjectOutput = {
    os.writeByte(ObjectStartMarker)
    streamObject
  }
}

private class StreamListOutput(os: DataOutputStream, output: StreamOutput) extends ListOutput {

  def writeElement(): Output = output

  def finish(): Unit = {
    os.writeByte(ListEndMarker)
  }
}

private class StreamObjectOutput(os: DataOutputStream, output: StreamOutput) extends ObjectOutput {

  def writeField(key: String): Output = {
    output.writeString(key)
    output
  }

  def finish(): Unit = {
    os.writeByte(ObjectEndMarker)
  }
}
