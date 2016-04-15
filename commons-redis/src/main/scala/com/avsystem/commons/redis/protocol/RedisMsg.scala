package com.avsystem.commons
package redis.protocol

import akka.util.{ByteString, ByteStringBuilder}
import com.avsystem.commons.misc.Opt
import com.avsystem.commons.redis.exception.InvalidDataException

import scala.collection.immutable.VectorBuilder
import scala.collection.mutable.ArrayBuffer

/**
  * Author: ghik
  * Created: 31/03/16.
  */
sealed trait RedisMsg
sealed trait ValidRedisMsg extends RedisMsg
case class SimpleStringMsg(string: ByteString) extends ValidRedisMsg {
  override def toString = s"$productPrefix(${RedisMsg.escape(string)})"
}
object SimpleStringMsg {
  def apply(str: String): SimpleStringMsg = SimpleStringMsg(ByteString(str))
}
case class ErrorMsg(errorString: ByteString) extends RedisMsg {
  override def toString = s"$productPrefix(${RedisMsg.escape(errorString)})"
  lazy val errorCode: String = errorString.indexOf(' '.toByte) match {
    case -1 => errorString.utf8String
    case i => errorString.slice(0, i).utf8String
  }
}
object ErrorMsg {
  def apply(str: String): ErrorMsg = ErrorMsg(ByteString(str))
}
case class IntegerMsg(value: Long) extends ValidRedisMsg
case object NullBulkStringMsg extends ValidRedisMsg
case class BulkStringMsg(string: ByteString) extends ValidRedisMsg {
  override def toString = s"$productPrefix(${RedisMsg.escape(string)})"
}
case object NullArrayMsg extends ValidRedisMsg
case class ArrayMsg(elements: IndexedSeq[RedisMsg]) extends ValidRedisMsg

object SimpleStringStr {
  def unapply(ss: SimpleStringMsg): Opt[String] =
    Opt(ss.string.utf8String)
}

object RedisMsg {
  def escape(bs: ByteString, quote: Boolean = true): String = {
    val sb = new StringBuilder(if(quote) "\"" else "")
    bs.foreach {
      case '\t' => sb ++= "\\r"
      case '\b' => sb ++= "\\b"
      case '\n' => sb ++= "\\n"
      case '\r' => sb ++= "\\r"
      case '\f' => sb ++= "\\f"
      case '\'' => sb ++= "\\'"
      case '\"' => sb ++= "\\"
      case '\\' => sb ++= "\\\\"
      case b if b > 0x1F && b < 0x7F => sb += b.toChar
      case b => sb ++= f"\\x$b%02x"
    }
    if(quote) {
      sb += '\"'
    }
    sb.result()
  }

  private final val CRLF = ByteString("\r\n")
  private final val MinusOne = ByteString("-1")

  private final val CRByte = '\r'.toByte
  private final val LFByte = '\n'.toByte
  private final val SimpleInd = '+'.toByte
  private final val ErrorInd = '-'.toByte
  private final val IntegerInd = ':'.toByte
  private final val BulkInd = '$'.toByte
  private final val ArrayInd = '*'.toByte

  def encode(msg: RedisMsg): ByteString = {
    val builder = new ByteStringBuilder
    encode(msg, builder)
    builder.result()
  }

  def encode(msgs: TraversableOnce[RedisMsg]): ByteString = {
    val builder = new ByteStringBuilder
    msgs.foreach(encode(_, builder))
    builder.result()
  }

  def encode(msg: RedisMsg, builder: ByteStringBuilder): Unit = {
    def encodeIn(msg: RedisMsg): Unit = msg match {
      case SimpleStringMsg(string) =>
        builder.putByte(SimpleInd).append(string).append(CRLF)
      case ErrorMsg(errorString) =>
        builder.putByte(ErrorInd).append(errorString).append(CRLF)
      case IntegerMsg(value: Long) =>
        builder.putByte(IntegerInd).append(ByteString(value.toString)).append(CRLF)
      case NullBulkStringMsg =>
        builder.putByte(BulkInd).append(MinusOne).append(CRLF)
      case BulkStringMsg(string) =>
        builder.putByte(BulkInd).append(ByteString(string.size.toString)).append(CRLF).append(string).append(CRLF)
      case NullArrayMsg =>
        builder.putByte(ArrayInd).append(MinusOne).append(CRLF)
      case ArrayMsg(elements) =>
        builder.putByte(ArrayInd).append(ByteString(elements.size.toString)).append(CRLF)
        elements.foreach(encodeIn)
    }
    encodeIn(msg)
  }

  def decode(bs: ByteString): Seq[RedisMsg] = {
    val builder = new VectorBuilder[RedisMsg]
    val decoder = new Decoder(builder += _)
    decoder.decodeMore(bs)
    builder.result()
  }

  object Decoder {
    private final val Initial = 0
    private final val ReadingSimple = 1
    private final val CREncountered = 2
    private final val StartingInt = 3
    private final val ReadingInt = 4
    private final val ReadingBulk = 5

    private final val ZeroDigitByte = '0'.toByte
    private final val NineDigitByte = '9'.toByte
    private final val MinusByte = '-'.toByte

    private object Digit {
      def unapply(b: Byte): Opt[Long] =
        if (b >= ZeroDigitByte && b <= NineDigitByte) Opt(b - ZeroDigitByte)
        else Opt.Empty
    }
  }

  final class Decoder(consumer: RedisMsg => Any) {

    import Decoder._

    private[this] var arrayStack: List[(Int, ArrayBuffer[RedisMsg])] = Nil
    private[this] var state: Int = Initial
    private[this] var currentType: Byte = 0
    private[this] var readingLength: Boolean = false
    private[this] var numberNegative: Boolean = false
    private[this] var numberValue: Long = 0
    private[this] val dataBuilder = new ByteStringBuilder

    private def completed(msg: RedisMsg): Unit = {
      arrayStack match {
        case Nil => consumer(msg)
        case (expected, collected) :: tail =>
          collected += msg
          if (collected.size >= expected) {
            arrayStack = tail
            completed(ArrayMsg(collected))
          }
      }
    }

    def fail(msg: String) = throw new InvalidDataException(msg)

    def decodeMore(bytes: ByteString): Unit = {
      def decode(idx: Int, prevDataStart: Int): Unit = if (idx < bytes.length) {
        val byte = bytes(idx)
        var dataStart = prevDataStart
        state match {
          case Initial =>
            currentType = byte
            byte match {
              case SimpleInd | ErrorInd =>
                state = ReadingSimple
              case IntegerInd =>
                state = StartingInt
              case BulkInd | ArrayInd =>
                state = StartingInt
                readingLength = true
              case _ => fail("Expected one of: '+', '-', ':', '$', '*'")
            }
          case StartingInt =>
            numberValue = 0
            state = ReadingInt
            byte match {
              case MinusByte =>
                numberNegative = true
              case Digit(digitValue) =>
                numberValue = digitValue
              case _ => fail("Expected '-' sign or digit")
            }
          case ReadingInt => byte match {
            case CRByte =>
              numberNegative = false
              state = CREncountered
            case Digit(digitValue) =>
              numberValue = numberValue * 10 + (if (numberNegative) -digitValue else digitValue)
            case _ => fail("Expected digit or CR")
          }
          case ReadingSimple =>
            if (dataStart < 0) {
              dataStart = idx
            }
            byte match {
              case CRByte =>
                dataBuilder.append(bytes.slice(dataStart, idx))
                dataStart = -1
                state = CREncountered
              case LFByte => fail("LF not allowed in simple string message")
              case _ =>
            }
          case ReadingBulk =>
            if (dataStart < 0) {
              dataStart = idx
            }
            if (dataBuilder.length + idx - dataStart == numberValue) {
              if (byte == CRByte) {
                dataBuilder.append(bytes.slice(dataStart, idx))
                dataStart = -1
                state = CREncountered
              } else fail("Expected CR at the end of bulk string message")
            }
          case CREncountered => byte match {
            case LFByte if readingLength =>
              readingLength = false
              currentType match {
                case BulkInd =>
                  numberValue match {
                    case -1 =>
                      state = Initial
                      completed(NullBulkStringMsg)
                    case size if size >= 0 =>
                      state = ReadingBulk
                    case _ => fail("Invalid bulk string length")
                  }
                case ArrayInd =>
                  state = Initial
                  numberValue match {
                    case -1 => completed(NullArrayMsg)
                    case 0 => completed(ArrayMsg(IndexedSeq.empty))
                    case size if size > 0 =>
                      val is = size.toInt
                      arrayStack = (is, new ArrayBuffer[RedisMsg](is)) :: arrayStack
                    case _ => fail("Invalid array size")
                  }
                case _ => fail("Length can be read only for bulk strings or arrays")
              }
            case LFByte =>
              def extractData() = {
                val res = dataBuilder.result()
                dataBuilder.clear()
                res
              }
              val msg = currentType match {
                case SimpleInd => SimpleStringMsg(extractData())
                case ErrorInd => ErrorMsg(extractData())
                case BulkInd => BulkStringMsg(extractData())
                case IntegerInd => IntegerMsg(numberValue)
              }
              completed(msg)
              state = Initial
            case _ => fail("Expected LF after CR")
          }
        }
        decode(idx + 1, dataStart)
      } else state match {
        case ReadingSimple | ReadingBulk if prevDataStart >= 0 =>
          dataBuilder.append(bytes.drop(prevDataStart))
        case _ =>
      }
      decode(0, -1)
    }
  }
}
