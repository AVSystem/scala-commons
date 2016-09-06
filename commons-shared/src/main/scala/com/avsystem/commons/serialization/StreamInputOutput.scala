package com.avsystem.commons
package serialization


import java.io.{ByteArrayOutputStream, DataInputStream, DataOutputStream, InputStream, IOException, OutputStream}
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
import PrimitiveSizes._


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
  private[this] val markerByte = is.readByte()
  private[serialization] val marker = Marker.of(markerByte).getOrElse(throw new IOException(s"Illegal marker $markerByte"))

  private[this] def checkedInternal[A](expected: Marker)(vr: => A): ValueRead[A] = {
    if (marker == expected) ReadSuccessful(vr) else ReadFailed(s"Expected $expected, but $marker found")
  }

  private[this] def checkedStatic[A](expected: StaticSize)(vr: => A): ValueRead[A] = checkedInternal(expected)(vr)

  private[this] def checkedDynamic[A](marker: DynamicSize)(vr: Int => A): ValueRead[A] = checkedInternal(marker) {
    vr(is.readInt())
  }

  override def readNull(): ValueRead[Null] = checkedStatic(NullMarker)(null)

  override def readString(): ValueRead[String] = checkedDynamic(StringMarker) { size =>
    val binaryString = Array.ofDim[Byte](size)
    is.readFully(binaryString)
    new String(binaryString, StandardCharsets.UTF_8)
  }

  override def readBoolean(): ValueRead[Boolean] = checkedStatic(BooleanMarker) {
    is.readBoolean()
  }

  override def readInt(): ValueRead[Int] = checkedStatic(IntMarker) {
    is.readInt()
  }

  override def readLong(): ValueRead[Long] = checkedStatic(LongMarker) {
    is.readLong()
  }

  override def readDouble(): ValueRead[Double] = checkedStatic(DoubleMarker) {
    is.readDouble()
  }

  override def readBinary(): ValueRead[Array[Byte]] = checkedDynamic(ByteArrayMarker) { size =>
    val binary = Array.ofDim[Byte](size)
    is.readFully(binary)
    binary
  }

  override def readList(): ValueRead[ListInput] = checkedDynamic(ListStartMarker) { size =>
    new StreamListInput(is)
  }

  override def readObject(): ValueRead[ObjectInput] = checkedDynamic(ObjectStartMarker) { size =>
    new StreamObjectInput(is)
  }

  override def skip(): Unit = {
    val toSkip  = marker match {
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
    currentInput.get.marker != ListEndMarker
  }
}

private class StreamObjectInput(is: DataInputStream) extends ObjectInput {
  import StreamObjectInput._

  private[this] var currentField: CurrentField = NoneYet

  private def ensureInput(): Unit = currentField match {
    case NoneYet =>
      val keyInput = new StreamInput(is)
      currentField = if (keyInput.marker != ObjectEndMarker) {
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

  private def writeStatic(marker: StaticSize)(f: => Unit) = {
    os.write(marker.byte)
    f
  }

  private def writeDynamic[A](marker: DynamicSize)(f: (DataOutputStream, () => Unit) => A): A = {
    val bytes = new ByteArrayOutputStream()
    val stream = new DataOutputStream(bytes)
    f(stream, { () =>
      os.write(marker.byte)
      os.writeInt(bytes.size())
      os.write(bytes.toByteArray)
    })
  }

  override def writeNull(): Unit = writeStatic(NullMarker)(())

  override def writeString(str: String): Unit =
    writeDynamic(StringMarker){ (stream, onFinish) =>
      stream.write(str.getBytes(StandardCharsets.UTF_8))
      onFinish()
    }

  override def writeBoolean(boolean: Boolean): Unit = writeStatic(BooleanMarker)(os.writeBoolean(boolean))

  override def writeInt(int: Int): Unit = writeStatic(IntMarker)(os.writeInt(int))

  override def writeLong(long: Long): Unit = writeStatic(LongMarker)(os.writeLong(long))

  override def writeDouble(double: Double): Unit = writeStatic(DoubleMarker)(os.writeDouble(double))

  override def writeBinary(binary: Array[Byte]): Unit = writeDynamic(ByteArrayMarker) { (stream, onFinish) =>
    stream.write(binary)
    onFinish()
  }

  override def writeList(): ListOutput = writeDynamic(ListStartMarker) { (stream, onFinish) =>
    new StreamListOutput(stream, onFinish)
  }

  override def writeObject(): ObjectOutput = writeDynamic(ObjectStartMarker) { (stream, onFinish) =>
    new StreamObjectOutput(stream, onFinish)
  }
}

private class StreamListOutput(os: DataOutputStream, onFinish: () => Unit) extends ListOutput {
  override def writeElement(): Output = new StreamOutput(os)

  override def finish(): Unit = {
    os.writeByte(ListEndMarker.byte)
    onFinish()
  }
}

private class StreamObjectOutput(os: DataOutputStream, onFinish: () => Unit) extends ObjectOutput {
  override def writeField(key: String): Output = {
    new StreamOutput(os).writeString(key)
    new StreamOutput(os)
  }

  override def finish(): Unit = {
    os.writeByte(ObjectEndMarker.byte)
    onFinish()
  }
}