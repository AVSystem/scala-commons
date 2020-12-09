package com.avsystem.commons
package serialization.cbor

import java.io.{ObjectInput => _, _}
import java.nio.charset.StandardCharsets

import com.avsystem.commons.serialization.GenCodec.ReadFailure
import com.avsystem.commons.serialization._

import scala.annotation.tailrec

final class CborReader(val data: RawCbor) {
  private[this] var idx = 0

  def index: Int = idx

  def reset(index: Int): Unit =
    idx = index

  def advance(amount: Int): Unit =
    idx += amount

  @tailrec def requireTag(cond: Tag => Boolean, error: String): Tag =
    InitialByte(data(idx)) match {
      case InitialByte(MajorType.Tag, info) =>
        advance(1)
        val tag = Tag(readUnsigned(info).toInt)
        if (cond(tag)) tag
        else requireTag(cond, error)
      case _ =>
        throw new ReadFailure(error)
    }

  @tailrec def nextInitial(): InitialByte =
    InitialByte(data(idx)) match {
      case InitialByte(MajorType.Tag, info) =>
        advance(1 + unsignedSize(info))
        nextInitial()
      case ib =>
        ib match {
          case InitialByte.IndefiniteLength(_) => openIndefs += 1
          case InitialByte.Break => openIndefs -= 1
          case _ =>
        }
        advance(1)
        ib
    }

  @tailrec def peekInitial(i: Int = idx): InitialByte =
    InitialByte(data(i)) match {
      case InitialByte(MajorType.Tag, info) =>
        peekInitial(i + 1 + unsignedSize(info))
      case ib => ib
    }

  @inline private def bits(off: Int): Long =
    data(idx + off).toLong & 0xFF

  def nextByte(): Long = {
    val res = bits(0)
    advance(1)
    res
  }

  def nextShort(): Long = {
    val res = (bits(0) << 8) | bits(1)
    advance(2)
    res
  }

  def nextInt(): Long = {
    val res = (bits(0) << 24) | (bits(1) << 16) | (bits(2) << 8) | bits(3)
    advance(4)
    res
  }

  def nextLong(): Long = {
    val res = (bits(0) << 56) | (bits(1) << 48) | (bits(2) << 40) | (bits(3) << 32) |
      (bits(4) << 24) | (bits(5) << 16) | (bits(6) << 8) | bits(7)
    advance(8)
    res
  }

  private[this] var openIndefs = 0 // number of currently open values of indefinite length

  def openIndefinites: Int =
    openIndefs

  /** Consumes any dangling Break bytes in order to close any unclosed nested indefinite length values. */
  @tailrec def closeIndefinites(expectedDepth: Int): Unit =
    if (openIndefs > expectedDepth) nextInitial() match {
      case InitialByte.Break => closeIndefinites(expectedDepth)
      case ib => unexpected(ib, "Break")
    }

  def readUnsigned(info: Int): Long = info match {
    case i if i < InitialByte.SingleByteValueInfo => i
    case InitialByte.SingleByteValueInfo => nextByte()
    case InitialByte.TwoBytesValueInfo => nextShort()
    case InitialByte.FourBytesValueInfo => nextInt()
    case InitialByte.EightBytesValueInfo => nextLong()
    case _ => throw new ReadFailure(s"Additional info $info does not represent following unsigned value")
  }

  def unsignedSize(info: Int): Int = info match {
    case i if i < InitialByte.SingleByteValueInfo => 0
    case InitialByte.SingleByteValueInfo => 1
    case InitialByte.TwoBytesValueInfo => 2
    case InitialByte.FourBytesValueInfo => 4
    case InitialByte.EightBytesValueInfo => 8
    case _ => throw new ReadFailure(s"Additional info $info does not represent following unsigned value")
  }

  def readSigned(major: MajorType, info: Int): Long = {
    val unsigned = readUnsigned(info)
    if (unsigned < 0) throw new ReadFailure(s"Unsigned value too large: ${ustr(unsigned)}")
    else if (major == MajorType.Unsigned) unsigned
    else -1 - unsigned
  }

  def ustr(unsigned: Long): String =
    java.lang.Long.toUnsignedString(unsigned)

  def validateSize(unsigned: Long, what: String): Int = {
    if (unsigned < 0 || unsigned > Int.MaxValue) {
      throw new ReadFailure(s"$what too large: ${ustr(unsigned)}")
    }
    unsigned.toInt
  }

  def readSizedText(info: Int): String = {
    val length = validateSize(readUnsigned(info), "text string length")
    val res = new String(data.bytes, data.offset + index, length, StandardCharsets.UTF_8)
    advance(length)
    res
  }

  def readSizedBytes(info: Int): Array[Byte] = {
    val length = validateSize(readUnsigned(info), "byte string length")
    val result = new Array[Byte](length)
    System.arraycopy(data.bytes, data.offset + index, result, 0, length)
    advance(length)
    result
  }

  def unexpected(ib: InitialByte, expected: String) =
    throw new ReadFailure(s"Unexpected initial byte: $ib, expected $expected")
}

object CborInput {
  def read[T: GenCodec](bytes: Array[Byte], fieldLabels: FieldLabels = FieldLabels.NoLabels): T = {
    GenCodec.read[T](RawCbor(bytes).createInput(fieldLabels))
  }

  private final val Two64 = BigInt(1) << 64
}

/**
  * An [[com.avsystem.commons.serialization.Input Input]] implementation that deserializes from
  * [[https://tools.ietf.org/html/rfc7049 CBOR]].
  */
class CborInput(reader: CborReader, fieldLabels: FieldLabels) extends InputAndSimpleInput {

  import reader._

  def readNull(): Boolean = peekInitial() match {
    case InitialByte.Null =>
      advance(1)
      true
    case _ =>
      false
  }

  def readString(): String = nextInitial() match {
    case InitialByte.IndefiniteLength(MajorType.TextString) =>
      val builder = new JStringBuilder
      @tailrec def readChunks(): Unit = nextInitial() match {
        case InitialByte.Break =>
        case InitialByte(MajorType.TextString, info) =>
          builder.append(readSizedText(info))
          readChunks()
        case ib =>
          unexpected(ib, "text string or break")
      }
      readChunks()
      builder.toString
    case InitialByte(MajorType.TextString, info) =>
      readSizedText(info)
    case ib =>
      unexpected(ib, "text string")
  }

  def readBoolean(): Boolean = nextInitial() match {
    case InitialByte.True => true
    case InitialByte.False => false
    case ib => unexpected(ib, "boolean")
  }

  def readInt(): Int =
    readLong().toInt

  def readLong(): Long = nextInitial() match {
    case InitialByte(major@(MajorType.Unsigned | MajorType.Negative), info) =>
      readSigned(major, info)
    case ib =>
      unexpected(ib, "integer")
  }

  def readDouble(): Double = nextInitial() match {
    case InitialByte(major@(MajorType.Unsigned | MajorType.Negative), info) =>
      readSigned(major, info).toDouble
    case InitialByte.HalfPrecisionFloat =>
      new HFloat(nextShort().toShort).toFloat.toDouble
    case InitialByte.SinglePrecisionFloat =>
      java.lang.Float.intBitsToFloat(nextInt().toInt)
    case InitialByte.DoublePrecisionFloat =>
      java.lang.Double.longBitsToDouble(nextLong())
    case ib =>
      unexpected(ib, "float")
  }

  def readBigInt(): BigInt = peekInitial() match {
    case InitialByte(major@(MajorType.Unsigned | MajorType.Negative), info) =>
      nextInitial()
      val uns = readUnsigned(info)
      val unsigned = if (uns >= 0) BigInt(uns) else CborInput.Two64 + uns
      if (major == MajorType.Unsigned) unsigned else -(unsigned + 1)
    case InitialByte(MajorType.ByteString, info) =>
      val tag = requireTag(t => t == Tag.PositiveBignum || t == Tag.NegativeBignum,
        "expected value tagged as positive or negative bignum")
      nextInitial()
      val unsigned = BigInt(readSizedBytes(info))
      if (tag == Tag.PositiveBignum) unsigned else -(unsigned + 1)
    case ib =>
      nextInitial()
      unexpected(ib, "integer or bignum")
  }

  def readBigDecimal(): BigDecimal = {
    requireTag(_ == Tag.DecimalFraction, "expected value tagged as decimal fraction")
    nextInitial() match {
      case InitialByte(MajorType.Array, 2) =>
        val scale = -readInt()
        val unscaled = readBigInt()
        BigDecimal(unscaled, scale)
      case ib =>
        unexpected(ib, s"two element array")
    }
  }

  def readBinary(): Array[Byte] = nextInitial() match {
    case InitialByte.IndefiniteLength(MajorType.ByteString) =>
      val out = new ByteArrayOutputStream
      @tailrec def readChunks(): Unit = nextInitial() match {
        case InitialByte.Break =>
        case InitialByte(MajorType.ByteString, info) =>
          out.write(readSizedBytes(info))
          readChunks()
        case ib => unexpected(ib, "byte string or break")
      }
      readChunks()
      out.toByteArray
    case InitialByte(MajorType.ByteString, info) =>
      readSizedBytes(info)
    case ib =>
      unexpected(ib, "byte string")
  }

  override def readTimestamp(): Long = {
    requireTag(_ == Tag.EpochDateTime, "expected value tagged as epoch date time (tag value 1)")
    val ib = peekInitial()
    ib.majorType match {
      case MajorType.Unsigned | MajorType.Negative => readLong() * 1000
      case MajorType.Simple => (readDouble() * 1000).toLong
      case _ =>
        nextInitial()
        unexpected(ib, s"integer or float")
    }
  }

  def readList(): CborListInput = nextInitial() match {
    case InitialByte.IndefiniteLength(MajorType.Array) =>
      new CborListInput(reader, -1, fieldLabels)
    case InitialByte(MajorType.Array, info) =>
      new CborListInput(reader, validateSize(readUnsigned(info), "array size"), fieldLabels)
    case ib =>
      unexpected(ib, "array")
  }

  def readObject(): CborObjectInput = nextInitial() match {
    case InitialByte.IndefiniteLength(MajorType.Map) =>
      new CborObjectInput(reader, -1, fieldLabels)
    case InitialByte(MajorType.Map, info) =>
      new CborObjectInput(reader, validateSize(readUnsigned(info), "map size"), fieldLabels)
    case ib =>
      unexpected(ib, "map")
  }

  def readRawCbor(): RawCbor = {
    val startIdx = index
    skip()
    RawCbor(data.bytes, data.offset + startIdx, index - startIdx)
  }

  def readTags(): List[Tag] = {
    val buf = new MListBuffer[Tag]
    val startIdx = index
    def loop(): Unit = peekInitial() match {
      case InitialByte(MajorType.Tag, info) =>
        nextInitial()
        buf += Tag(readUnsigned(info).toInt)
        loop()
      case _ =>
        reset(startIdx)
    }
    loop()
    buf.result()
  }

  override def readCustom[T](typeMarker: TypeMarker[T]): Opt[T] = typeMarker match {
    case RawCbor =>
      Opt(readRawCbor())
    case _ =>
      super.readCustom(typeMarker)
  }

  override def readMetadata[T](metadata: InputMetadata[T]): Opt[T] = metadata match {
    case InitialByte => Opt(peekInitial())
    case Tags => Opt(readTags())
    case _ => super.readMetadata(metadata)
  }

  @tailrec private def skipUntilBreak(): Unit = nextInitial() match {
    case InitialByte.Break =>
    case ib =>
      skip(ib)
      skipUntilBreak()
  }

  @tailrec private def skipElems(num: Int): Unit =
    if (num > 0) {
      skip(nextInitial())
      skipElems(num - 1)
    }

  private def skip(initialByte: InitialByte): Unit = {
    import MajorType._
    initialByte match {
      case InitialByte(Unsigned | Negative, info) =>
        advance(unsignedSize(info))
      case InitialByte.HalfPrecisionFloat => advance(2)
      case InitialByte.SinglePrecisionFloat => advance(4)
      case InitialByte.DoublePrecisionFloat => advance(8)
      case InitialByte.IndefiniteLength(_) =>
        skipUntilBreak()
      case InitialByte(TextString | ByteString, info) =>
        advance(validateSize(readUnsigned(info), "text or byte string length"))
      case InitialByte(Array, info) =>
        skipElems(validateSize(readUnsigned(info), "array length"))
      case InitialByte(Map, info) =>
        skipElems(validateSize(readUnsigned(info), "map length") * 2)
      case InitialByte.Break =>
        throw new ReadFailure(s"unexpected Break")
      case _ =>
    }
  }

  def skip(): Unit =
    skip(nextInitial())
}

class CborFieldInput(val fieldName: String, reader: CborReader, fieldLabels: FieldLabels)
  extends CborInput(reader, fieldLabels) with FieldInput

abstract class CborSequentialInput(reader: CborReader, private[this] var size: Int) extends SequentialInput {

  private val indefDepth = reader.openIndefinites

  override def knownSize: Int = size

  def hasNext: Boolean = {
    reader.closeIndefinites(indefDepth)
    size match {
      case -1 =>
        reader.peekInitial() match {
          case InitialByte.Break => false
          case _ => true
        }
      case i => i > 0
    }
  }

  protected def prepareForNext(): Unit =
    if (!hasNext)
      throw new NoSuchElementException
    else if (size > 0) {
      size -= 1
    }
}

class CborListInput(reader: CborReader, size: Int, fieldLabels: FieldLabels)
  extends CborSequentialInput(reader, size) with ListInput {

  def nextElement(): CborInput = {
    prepareForNext()
    new CborInput(reader, fieldLabels)
  }
}

class CborObjectInput(reader: CborReader, size: Int, fieldLabels: FieldLabels)
  extends CborSequentialInput(reader, size) with ObjectInput {

  import reader._

  def nextField(): CborFieldInput = {
    prepareForNext()
    val fieldName = nextInitial() match {
      case InitialByte(major@(MajorType.Unsigned | MajorType.Negative), info) =>
        val label = readSigned(major, info).toInt
        fieldLabels.field(label).getOrElse(throw new ReadFailure(s"unknown field label $label"))
      case InitialByte(MajorType.TextString, info) =>
        readSizedText(info)
      case ib =>
        unexpected(ib, "integer or text string")
    }
    new CborFieldInput(fieldName, reader, fieldLabels)
  }
}
