package com.avsystem.commons
package serialization.cbor

import com.avsystem.commons.misc.{AbstractValueEnum, AbstractValueEnumCompanion, EnumCtx}
import com.avsystem.commons.serialization.InputMetadata

/** [[https://tools.ietf.org/html/rfc7049#section-2.1]] */
final class MajorType(implicit enumCtx: EnumCtx) extends AbstractValueEnum {
  def withInfo(info: Int): InitialByte =
    InitialByte(this, info)
}
object MajorType extends AbstractValueEnumCompanion[MajorType] {
  final val Unsigned, Negative, ByteString, TextString, Array, Map, Tag, Simple: Value = new MajorType
}

/** [[https://tools.ietf.org/html/rfc7049#section-2]] */
final class InitialByte(val value: Byte) extends AnyVal {
  def majorType: MajorType = MajorType.values((value & 0xff) >>> 5)
  def additionalInfo: Int = value & 0x1f

  override def toString: String = s"major type $majorType with info $additionalInfo"
}
object InitialByte extends InputMetadata[InitialByte] {

  import MajorType.*

  def apply(major: MajorType, info: Int): InitialByte = {
    require(info >= 0 && info < 32)
    InitialByte(((major.ordinal << 5) | info).toByte)
  }

  def apply(byte: Byte): InitialByte =
    new InitialByte(byte)

  def unapply(ib: InitialByte): Opt[(MajorType, Int)] =
    Opt((ib.majorType, ib.additionalInfo))

  final val SingleByteValueInfo = 24
  final val TwoBytesValueInfo = 25
  final val FourBytesValueInfo = 26
  final val EightBytesValueInfo = 27
  final val IndefiniteLengthInfo = 31

  object IndefiniteLength {
    def apply(major: MajorType): InitialByte = {
      require(major.ordinal >= MajorType.ByteString.ordinal && major.ordinal <= MajorType.Map.ordinal)
      InitialByte(major, IndefiniteLengthInfo)
    }

    def unapply(byte: InitialByte): Opt[MajorType] =
      if (
        byte.additionalInfo == IndefiniteLengthInfo && byte.majorType.ordinal >= MajorType.ByteString.ordinal &&
        byte.majorType.ordinal <= MajorType.Map.ordinal
      )
        Opt(byte.majorType)
      else Opt.Empty
  }

  // https://tools.ietf.org/html/rfc7049#section-2.3
  final val False: InitialByte = Simple.withInfo(20)
  final val True: InitialByte = Simple.withInfo(21)
  final val Null: InitialByte = Simple.withInfo(22)
  final val Undefined: InitialByte = Simple.withInfo(23)
  final val HalfPrecisionFloat: InitialByte = Simple.withInfo(25)
  final val SinglePrecisionFloat: InitialByte = Simple.withInfo(26)
  final val DoublePrecisionFloat: InitialByte = Simple.withInfo(27)
  final val Break: InitialByte = Simple.withInfo(31)
}

/** [[https://tools.ietf.org/html/rfc7049#section-2.4]] [[https://www.iana.org/assignments/cbor-tags/cbor-tags.xhtml]]
  */
//case class Tag(value: Int) extends AnyVal
//object Tag extends IntWrapperCompanion[Tag] {
//  final val StandardDateTime: Tag = Tag(0)
//  final val EpochDateTime: Tag = Tag(1)
//  final val PositiveBignum: Tag = Tag(2)
//  final val NegativeBignum: Tag = Tag(3)
//  final val DecimalFraction: Tag = Tag(4)
//  final val Bigfloat: Tag = Tag(5)
//  final val ExpectedBase64Url: Tag = Tag(21)
//  final val ExpectedBase64: Tag = Tag(22)
//  final val ExpectedBase16: Tag = Tag(23)
//  final val EncodedDataItem: Tag = Tag(24)
//  final val Uri: Tag = Tag(32)
//  final val Base64Url: Tag = Tag(33)
//  final val Base64: Tag = Tag(34)
//  final val Regexp: Tag = Tag(35)
//  final val MimeMessage: Tag = Tag(36)
//  final val SelfDescribe: Tag = Tag(55799)
//}
//
//object Tags extends InputMetadata[List[Tag]]
//todo: wtf