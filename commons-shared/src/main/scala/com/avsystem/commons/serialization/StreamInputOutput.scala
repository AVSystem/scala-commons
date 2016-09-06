package com.avsystem.commons
package serialization


import java.io.{ByteArrayOutputStream, DataInputStream, DataOutputStream, IOException}
import java.nio.charset.StandardCharsets
import java.{lang => jl}

import com.avsystem.commons.misc.{Opt, SealedEnumCompanion}


private object PrimitiveSizes {
  val ByteBytes = jl.Byte.BYTES
  val ShortBytes = jl.Short.BYTES
  val IntBytes = jl.Integer.BYTES
  val LongBytes = jl.Long.BYTES
  val FloatBytes = jl.Float.BYTES
  val DoubleBytes = jl.Double.BYTES
}

import com.avsystem.commons.serialization.PrimitiveSizes._


private sealed abstract class Marker(val byte: Byte)
private sealed abstract class StaticSize(val size: Int, byte: Byte) extends Marker(byte)
private sealed abstract class DynamicSize(byte: Byte) extends Marker(byte)
private sealed trait BiMarker {
  def byte: Byte
}


private case object NullMarker extends StaticSize(0, 0)
private case object StringMarker extends DynamicSize(1)
private case object ByteMarker extends StaticSize(ByteBytes, 2)
private case object ShortMarker extends StaticSize(ShortBytes, 3)
private case object IntMarker extends StaticSize(IntBytes, 4)
private case object LongMarker extends StaticSize(LongBytes, 5)
private case object FloatMarker extends StaticSize(FloatBytes, 6)
private case object DoubleMarker extends StaticSize(DoubleBytes, 7)
private case object ByteArrayMarker extends DynamicSize(8)
private case object BooleanMarker extends StaticSize(ByteBytes, 9)
private case object ListStartMarker extends DynamicSize(10) with BiMarker
private case object ObjectStartMarker extends DynamicSize(11) with BiMarker
private case object ListEndMarker extends Marker(12)
private case object ObjectEndMarker extends Marker(13)

private object Marker extends SealedEnumCompanion[Marker] {
  val values: List[Marker] = caseObjects

  private val markers = values.toArray.sortBy(_.byte)

  def of(byte: Byte): Opt[Marker] =
    if (byte >= 0 && byte < markers.length) Opt(markers(byte)) else Opt.empty
}

class StreamInput(is: DataInputStream) extends Input {
  private[serialization] val markerByte = is.readByte()

  override def readNull(): ValueRead[Null] = if (markerByte == NullMarker.byte) ReadSuccessful(null) else ReadFailed(s"Expected string, but $markerByte found")

  override def readString(): ValueRead[String] = if (markerByte == StringMarker.byte) ReadSuccessful({
    val binaryString = Array.ofDim[Byte](is.readInt())
    is.readFully(binaryString)
    new String(binaryString, StandardCharsets.UTF_8)
  })
  else ReadFailed(s"Expected string, but $markerByte found")

  override def readBoolean(): ValueRead[Boolean] = if (markerByte == BooleanMarker.byte) ReadSuccessful({
    is.readBoolean()
  })
  else ReadFailed(s"Expected boolean, but $markerByte found")

  override def readInt(): ValueRead[Int] = if (markerByte == IntMarker.byte) ReadSuccessful({
    is.readInt()
  })
  else ReadFailed(s"Expected int, but $markerByte found")

  override def readLong(): ValueRead[Long] = if (markerByte == LongMarker.byte) ReadSuccessful({
    is.readLong()
  })
  else ReadFailed(s"Expected long, but $markerByte found")

  override def readDouble(): ValueRead[Double] = if (markerByte == DoubleMarker.byte) ReadSuccessful({
    is.readDouble()
  })
  else ReadFailed(s"Expected double, but $markerByte found")

  override def readBinary(): ValueRead[Array[Byte]] = if (markerByte == ByteArrayMarker.byte) ReadSuccessful({
    val binary = Array.ofDim[Byte](is.readInt())
    is.readFully(binary)
    binary
  })
  else ReadFailed(s"Expected binary array, but $markerByte found")

  override def readList(): ValueRead[ListInput] = if (markerByte == ListStartMarker.byte) ReadSuccessful({
    is.readInt()
    new StreamListInput(is)
  })
  else ReadFailed(s"Expected list, but $markerByte found")

  override def readObject(): ValueRead[ObjectInput] = if (markerByte == ObjectStartMarker.byte) ReadSuccessful({
    is.readInt()
    new StreamObjectInput(is)
  })
  else ReadFailed(s"Expected object, but $markerByte found")

  override def skip(): Unit = {
    val toSkip = Marker.of(markerByte).get match {
      case m: StaticSize =>
        m.size
      case m: DynamicSize =>
        is.readInt()
      case ListEndMarker =>
        throw new IOException("Illegal list end marker")
      case ObjectEndMarker =>
        throw new IOException("Illegal object end marker")
    }
    is.skipBytes(toSkip)
  }
}

private class StreamListInput(is: DataInputStream) extends ListInput {
  private[this] var currentInput: Opt[StreamInput] = Opt.empty

  private def ensureInput(): Unit =
    if (currentInput == Opt.empty) currentInput = Opt.some(new StreamInput(is))

  override def nextElement(): Input = {
    if (!hasNext) throw new IllegalStateException("List already emptied")
    val input = currentInput
    currentInput = Opt.empty
    input.get
  }

  override def hasNext: Boolean = {
    ensureInput()
    currentInput.get.markerByte != ListEndMarker.byte
  }
}

private class StreamObjectInput(is: DataInputStream) extends ObjectInput {

  import StreamObjectInput._

  private[this] var currentField: CurrentField = NoneYet

  private def ensureInput(): Unit = currentField match {
    case NoneYet =>
      val keyInput = new StreamInput(is)
      currentField = if (keyInput.markerByte != ObjectEndMarker.byte) {
        val keyString = keyInput.readString().get
        val valueInput = new StreamInput(is)
        Field(keyString, valueInput)
      } else {
        End
      }
    case End =>
    case _: Field =>
  }

  override def nextField(): (String, Input) = {
    if (!hasNext) throw new IllegalStateException("Object already emptied")
    val Field(key, field) = currentField
    currentField = NoneYet
    (key, field)
  }

  override def hasNext: Boolean = {
    ensureInput()
    currentField != End
  }
}

private object StreamObjectInput {
  private sealed trait CurrentField
  private case object NoneYet extends CurrentField
  private case object End extends CurrentField
  private case class Field(key: String, value: StreamInput) extends CurrentField
}

class StreamOutput(os: DataOutputStream) extends Output {

  override def writeNull(): Unit = os.write(NullMarker.byte)

  override def writeString(str: String): Unit = {
    os.writeByte(StringMarker.byte)
    val binary = str.getBytes(StandardCharsets.UTF_8)
    os.writeInt(binary.length)
    os.write(binary)
  }

  override def writeBoolean(boolean: Boolean): Unit = {
    os.writeByte(BooleanMarker.byte)
    os.writeBoolean(boolean)
  }

  override def writeInt(int: Int): Unit = {
    os.writeByte(IntMarker.byte)
    os.writeInt(int)
  }

  override def writeLong(long: Long): Unit = {
    os.writeByte(LongMarker.byte)
    os.writeLong(long)
  }

  override def writeDouble(double: Double): Unit = {
    os.writeByte(DoubleMarker.byte)
    os.writeDouble(double)
  }

  override def writeBinary(binary: Array[Byte]): Unit = {
    os.writeByte(ByteArrayMarker.byte)
    os.writeInt(binary.length)
    os.write(binary)
  }

  override def writeList(): ListOutput = {
    os.writeByte(ListStartMarker.byte)
    new StreamListOutput(os)
  }

  override def writeObject(): ObjectOutput = {
    os.writeByte(ObjectStartMarker.byte)
    new StreamObjectOutput(os)
  }
}

private class StreamListOutput(os: DataOutputStream) extends ListOutput {
  private[this] val innerOs = new ByteArrayOutputStream()
  private[this] val innerDataOs = new DataOutputStream(innerOs)

  override def writeElement(): Output = new StreamOutput(innerDataOs)

  override def finish(): Unit = {
    os.writeInt(innerOs.size() + 1)
    os.write(innerOs.toByteArray)
    os.writeByte(ListEndMarker.byte)
  }
}

private class StreamObjectOutput(os: DataOutputStream) extends ObjectOutput {
  private[this] val innerOs = new ByteArrayOutputStream()
  private[this] val innerDataOs = new DataOutputStream(innerOs)

  override def writeField(key: String): Output = {
    new StreamOutput(innerDataOs).writeString(key)
    new StreamOutput(innerDataOs)
  }

  override def finish(): Unit = {
    os.writeInt(innerOs.size() + 1)
    os.write(innerOs.toByteArray)
    os.writeByte(ObjectEndMarker.byte)
  }
}
