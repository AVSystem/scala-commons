package com.avsystem.commons
package redis.protocol

import org.apache.pekko.util.ByteString
import scala.annotation.nowarn
import org.scalacheck.util.Buildable
import org.scalacheck.{Arbitrary, Gen, Shrink}

/**
  * Author: ghik
  * Created: 04/04/16.
  */
object RedisMsgScalacheck {
  implicit val byteStringBuildable: Buildable[Byte, ByteString] =
    new Buildable[Byte, ByteString] {
      def builder: MBuilder[Byte, ByteString] = ByteString.newBuilder
    }

  implicit val shrinkSimpleString: Shrink[SimpleStringMsg] =
    Shrink(ss => Shrink.shrink(ss.string).map(SimpleStringMsg(_)))

  implicit val shrinkError: Shrink[ErrorMsg] =
    Shrink(err => Shrink.shrink(err.errorString).map(ErrorMsg(_)))

  implicit val shrinkBulkString: Shrink[BulkStringMsg] =
    Shrink(bs => Shrink.shrink(bs.string).map(BulkStringMsg(_)))

  implicit val shrinkArray: Shrink[ArrayMsg[RedisMsg]] =
    Shrink(arr => Shrink.shrink(arr.elements).map(ArrayMsg(_)))

  @nowarn("msg=deprecated")
  implicit val shrinkRedisProtocolMsg: Shrink[RedisMsg] = Shrink {
    case ss: SimpleStringMsg => Shrink.shrink(ss)
    case er: ErrorMsg => Shrink.shrink(er)
    case NullBulkStringMsg => Stream.empty
    case bs: BulkStringMsg => Shrink.shrink(bs)
    case im: IntegerMsg => Shrink.shrink(im)
    case NullArrayMsg => Stream.empty
    case am: ArrayMsg[RedisMsg] => Shrink.shrink(am)
  }

  val simpleBytes = (Byte.MinValue.toInt to Byte.MaxValue.toInt)
    .filter(b => b != '\n'.toInt && b != '\r'.toInt).map(_.toByte)

  def byteGen = Gen.chooseNum(Byte.MinValue, Byte.MaxValue, '\r'.toByte, '\n'.toByte)
  def simpleByteGen = Gen.oneOf(simpleBytes)

  def bytesGen: Gen[ByteString] = Gen.buildableOf[ByteString, Byte](byteGen)
  def simpleBytesGen: Gen[ByteString] = Gen.buildableOf[ByteString, Byte](simpleByteGen)

  def simpleStringGen = simpleBytesGen.map(SimpleStringMsg(_))
  def errorGen = simpleBytesGen.map(ErrorMsg(_))
  def integerGen = Arbitrary.arbitrary[Long].map(IntegerMsg(_))

  def bulkStringGen: Gen[RedisMsg] = Gen.sized(s => Gen.choose(-1, s).flatMap {
    case -1 => Gen.const(NullBulkStringMsg)
    case n => Gen.buildableOfN[ByteString, Byte](n, byteGen).map(bs => BulkStringMsg(bs))
  })

  def arrayGen: Gen[RedisMsg] = Gen.sized(s => Gen.choose(-1, s).flatMap {
    case -1 => Gen.const(NullArrayMsg)
    case 0 => Gen.const(ArrayMsg(IndexedSeq.empty))
    case n => Gen.buildableOfN[IndexedSeq[RedisMsg], RedisMsg](n, Gen.resize(s / n, redisProtocolMsgGen))
      .map(els => ArrayMsg(els))
  })

  def redisProtocolMsgGen: Gen[RedisMsg] =
    Gen.oneOf(simpleStringGen, errorGen, integerGen, bulkStringGen, arrayGen)
}
