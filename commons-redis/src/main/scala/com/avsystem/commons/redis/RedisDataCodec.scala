package com.avsystem.commons
package redis

import akka.util.ByteString
import com.avsystem.commons.misc.{NamedEnum, NamedEnumCompanion}
import com.avsystem.commons.redis.util.ByteStringSerialization
import com.avsystem.commons.serialization.GenCodec

/**
  *
  */
case class RedisDataCodec[T](read: ByteString => T, write: T => ByteString)
object RedisDataCodec extends LowPriorityRedisDataCodecs {
  def apply[T](implicit rdc: RedisDataCodec[T]): RedisDataCodec[T] = rdc

  def write[T](value: T)(implicit rdc: RedisDataCodec[T]): ByteString = rdc.write(value)
  def read[T](raw: ByteString)(implicit rdc: RedisDataCodec[T]): T = rdc.read(raw)

  implicit val ByteStringCodec: RedisDataCodec[ByteString] = RedisDataCodec(identity, identity)
  implicit val ByteArrayCodec: RedisDataCodec[Array[Byte]] = RedisDataCodec(_.toArray, ByteString(_))
  implicit val StringCodec: RedisDataCodec[String] = RedisDataCodec(_.utf8String, ByteString(_))
  implicit val BooleanKeyCodec: RedisDataCodec[Boolean] = RedisDataCodec(bs => bs.utf8String.toInt != 0, b => ByteString(if (b) "1" else "0"))
  implicit val CharCodec: RedisDataCodec[Char] = RedisDataCodec(_.utf8String.charAt(0), v => ByteString(v.toString))
  implicit val ByteCodec: RedisDataCodec[Byte] = RedisDataCodec(_.utf8String.toByte, v => ByteString(v.toString))
  implicit val ShortCodec: RedisDataCodec[Short] = RedisDataCodec(_.utf8String.toShort, v => ByteString(v.toString))
  implicit val IntCodec: RedisDataCodec[Int] = RedisDataCodec(_.utf8String.toInt, v => ByteString(v.toString))
  implicit val LongCodec: RedisDataCodec[Long] = RedisDataCodec(_.utf8String.toLong, v => ByteString(v.toString))
  implicit val FloatCodec: RedisDataCodec[Float] = RedisDataCodec(_.utf8String.toFloat, v => ByteString(v.toString))
  implicit val DoubleCodec: RedisDataCodec[Double] = RedisDataCodec(_.utf8String.toDouble, v => ByteString(v.toString))
  implicit def namedEnumCodec[E <: NamedEnum](implicit companion: NamedEnumCompanion[E]): RedisDataCodec[E] =
    RedisDataCodec(bs => companion.byName(bs.utf8String), v => ByteString(v.name))

}
trait LowPriorityRedisDataCodecs { this: RedisDataCodec.type =>
  implicit def fromGenCodec[T: GenCodec]: RedisDataCodec[T] =
    RedisDataCodec(bytes => ByteStringSerialization.read(bytes), value => ByteStringSerialization.write(value))
}
