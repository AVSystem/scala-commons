package com.avsystem.commons
package serialization.cbor

import java.io.{ByteArrayOutputStream, DataOutput, DataOutputStream}
import java.nio.charset.StandardCharsets

import com.avsystem.commons.serialization.GenCodec.WriteFailure
import com.avsystem.commons.serialization._

/**
  * Defines translation between textual object field names and corresponding numeric labels. May be used to reduce
  * size of CBOR representation of objects.
  */
trait FieldLabels {
  def label(field: String): Opt[Int]
  def field(label: Int): Opt[String]
}
object FieldLabels {
  final val NoLabels: FieldLabels = new FieldLabels {
    def label(field: String): Opt[Int] = Opt.Empty
    def field(label: Int): Opt[String] = Opt.Empty
  }
}

abstract class BaseCborOutput(out: DataOutput) {
  protected final def write(byte: InitialByte): Unit =
    out.write(byte.value)

  private def unsignedInfo(unsignedBytes: Long): Int =
    if (unsignedBytes >= 0 && unsignedBytes < InitialByte.SingleByteValueInfo) unsignedBytes.toInt
    else if ((unsignedBytes & 0xFFL) == unsignedBytes) InitialByte.SingleByteValueInfo
    else if ((unsignedBytes & 0xFFFFL) == unsignedBytes) InitialByte.TwoBytesValueInfo
    else if ((unsignedBytes & 0xFFFFFFFFL) == unsignedBytes) InitialByte.FourBytesValueInfo
    else InitialByte.EightBytesValueInfo

  // unsignedBytes represents 8-byte unsigned integer
  protected final def writeValue(major: MajorType, unsignedBytes: Long): Unit = {
    val info = unsignedInfo(unsignedBytes)
    write(InitialByte(major, info))
    info match {
      case InitialByte.SingleByteValueInfo => out.writeByte(unsignedBytes.toInt)
      case InitialByte.TwoBytesValueInfo => out.writeShort(unsignedBytes.toInt)
      case InitialByte.FourBytesValueInfo => out.writeInt(unsignedBytes.toInt)
      case InitialByte.EightBytesValueInfo => out.writeLong(unsignedBytes)
      case _ =>
    }
  }

  def writeSigned(value: Long): Unit =
    if (value >= 0) writeValue(MajorType.Unsigned, value)
    else writeValue(MajorType.Negative, -(value + 1))

  protected final def writeTag(tag: Tag): Unit =
    writeValue(MajorType.Tag, tag.value)

  protected final def writeText(str: String): Unit = {
    val bytes = str.getBytes(StandardCharsets.UTF_8)
    writeValue(MajorType.TextString, bytes.length)
    out.write(bytes)
  }
}

object CborOutput {
  def write[T: GenCodec](
    value: T,
    fieldLabels: FieldLabels = FieldLabels.NoLabels,
    sizePolicy: SizePolicy = SizePolicy.Optional
  ): Array[Byte] = {
    val baos = new ByteArrayOutputStream
    GenCodec.write[T](new CborOutput(new DataOutputStream(baos), fieldLabels, sizePolicy), value)
    baos.toByteArray
  }
}

/**
  * An [[com.avsystem.commons.serialization.Output Output]] implementation that serializes into
  * [[https://tools.ietf.org/html/rfc7049 CBOR]].
  */
class CborOutput(out: DataOutput, fieldLabels: FieldLabels, sizePolicy: SizePolicy)
  extends BaseCborOutput(out) with OutputAndSimpleOutput {

  def writeNull(): Unit =
    write(InitialByte.Null)

  def writeBoolean(boolean: Boolean): Unit =
    write(if (boolean) InitialByte.True else InitialByte.False)

  def writeString(str: String): Unit =
    writeText(str)

  def writeInt(int: Int): Unit =
    writeLong(int)

  def writeLong(long: Long): Unit =
    writeSigned(long)

  def writeDouble(double: Double): Unit =
    if (double.toLong.toDouble == double) {
      writeLong(double.toLong)
    } else if (double.isNaN) {
      write(InitialByte.HalfPrecisionFloat)
      out.writeShort(HFloat.NaN.raw)
    } else {
      val float = double.toFloat
      if (float.toDouble == double) {
        val hfloat = HFloat.fromFloat(float)
        if (hfloat.toFloat == float) {
          write(InitialByte.HalfPrecisionFloat)
          out.writeShort(hfloat.raw)
        } else {
          write(InitialByte.SinglePrecisionFloat)
          out.writeFloat(float)
        }
      }
      else {
        write(InitialByte.DoublePrecisionFloat)
        out.writeDouble(double)
      }
    }

  def writeBigInt(bigInt: BigInt): Unit = {
    val neg = bigInt < 0
    val unsigned = if (neg) -(bigInt + 1) else bigInt
    if (unsigned.bitLength <= 64) {
      writeValue(if (neg) MajorType.Negative else MajorType.Unsigned, unsigned.longValue)
    } else {
      writeTag(if (neg) Tag.NegativeBignum else Tag.PositiveBignum)
      writeBinary(unsigned.toByteArray)
    }
  }

  def writeBigDecimal(bigDecimal: BigDecimal): Unit = {
    writeTag(Tag.DecimalFraction)
    writeValue(MajorType.Array, 2)
    writeSigned(-bigDecimal.scale)
    writeBigInt(bigDecimal.bigDecimal.unscaledValue)
  }

  def writeBinary(binary: Array[Byte]): Unit = {
    writeValue(MajorType.ByteString, binary.length)
    out.write(binary)
  }

  override def writeTimestamp(millis: Long): Unit = {
    writeTag(Tag.EpochDateTime)
    if (millis % 1000 == 0)
      writeLong(millis / 1000)
    else
      writeDouble(millis.toDouble / 1000)
  }

  def writeList(): ListOutput =
    new CborListOutput(out, fieldLabels, sizePolicy)

  def writeObject(): ObjectOutput =
    new CborObjectOutput(out, fieldLabels, sizePolicy)

  def writeRawCbor(raw: RawCbor): Unit =
    out.write(raw.bytes, raw.offset, raw.length)

  override def writeCustom[T](typeMarker: TypeMarker[T], value: T): Boolean =
    typeMarker match {
      case RawCbor =>
        writeRawCbor(value)
        true
      case _ =>
        super.writeCustom(typeMarker, value)
    }

  override def keepsMetadata(metadata: InputMetadata[_]): Boolean = metadata match {
    case InitialByte | Tags => true
    case _ => super.keepsMetadata(metadata)
  }
}

abstract class CborSequentialOutput(
  out: DataOutput,
  override val sizePolicy: SizePolicy
) extends BaseCborOutput(out) with SequentialOutput {

  protected[this] var size: Int = -1
  protected[this] var fresh: Boolean = true

  protected final def writeInitial(major: MajorType): Unit =
    if (fresh) {
      fresh = false
      if (size >= 0) {
        writeValue(major, size)
      } else if (sizePolicy != SizePolicy.Required) {
        write(InitialByte(major, InitialByte.IndefiniteLengthInfo))
      } else {
        throw new WriteFailure("explicit size for an array or object was required but it was not declared")
      }
    }

  override final def declareSize(size: Int): Unit =
    if (fresh) {
      this.size = size
    } else {
      throw new IllegalStateException("Cannot declare size after elements or fields have already been written")
    }
}

class CborListOutput(
  out: DataOutput,
  fieldLabels: FieldLabels,
  sizePolicy: SizePolicy
) extends CborSequentialOutput(out, sizePolicy) with ListOutput {

  def writeElement(): Output = {
    writeInitial(MajorType.Array)
    if (size > 0) {
      size -= 1
    } else if (size == 0) {
      throw new WriteFailure("explicit size was given and all the elements have already been written")
    }
    new CborOutput(out, fieldLabels, sizePolicy)
  }

  def finish(): Unit = {
    writeInitial(MajorType.Array)
    if (size < 0) {
      write(InitialByte.Break)
    } else if (size > 0) {
      throw new WriteFailure("explicit size was given but not enough elements were written")
    }
  }
}

class CborObjectOutput(
  out: DataOutput,
  fieldLabels: FieldLabels,
  sizePolicy: SizePolicy
) extends CborSequentialOutput(out, sizePolicy) with ObjectOutput {

  def writeField(key: String): Output = {
    writeInitial(MajorType.Map)
    if (size > 0) {
      size -= 1
    } else if (size == 0) {
      throw new WriteFailure("explicit size was given and all the fields have already been written")
    }
    fieldLabels.label(key) match {
      case Opt(label) => writeSigned(label)
      case Opt.Empty => writeText(key)
    }
    new CborOutput(out, fieldLabels, sizePolicy)
  }

  def finish(): Unit = {
    writeInitial(MajorType.Map)
    if (size < 0) {
      write(InitialByte.Break)
    } else if (size > 0) {
      throw new WriteFailure("explicit size was given but not enough fields were written")
    }
  }
}
